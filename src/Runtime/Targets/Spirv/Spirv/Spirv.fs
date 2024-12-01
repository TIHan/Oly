// File is generated. Do not modify.
[<AutoOpen>]
module rec Spirv.Core

// Do not warn if we do not handle all cases in a match statement. It's fine that it will throw an exception.
#nowarn "104"

open System
open System.IO
open InternalHelpers

// https://github.com/KhronosGroup/SPIRV-Headers/blob/master/include/spirv/unified1/spirv.core.grammar.json

[<Literal>] 
let MagicNumber = 0x07230203u

[<RequireQualifiedAccess>]
type ImageOperands =
    | None
    | Bias of IdRef
    | Lod of IdRef
    | Grad of IdRef * IdRef
    | ConstOffset of IdRef
    | Offset of IdRef
    | ConstOffsets of IdRef
    | Sample of IdRef
    | MinLod of IdRef
    | MakeTexelAvailable of IdScope
    | MakeTexelVisible of IdScope
    | NonPrivateTexel
    | VolatileTexel
    | SignExtend
    | ZeroExtend
    | Nontemporal
    | Offsets of IdRef

    member x.Value =
       match x with
       | None -> 0x0000u
       | Bias _ -> 0x0001u
       | Lod _ -> 0x0002u
       | Grad _ -> 0x0004u
       | ConstOffset _ -> 0x0008u
       | Offset _ -> 0x0010u
       | ConstOffsets _ -> 0x0020u
       | Sample _ -> 0x0040u
       | MinLod _ -> 0x0080u
       | MakeTexelAvailable _ -> 0x0100u
       | MakeTexelVisible _ -> 0x0200u
       | NonPrivateTexel -> 0x0400u
       | VolatileTexel -> 0x0800u
       | SignExtend -> 0x1000u
       | ZeroExtend -> 0x2000u
       | Nontemporal -> 0x4000u
       | Offsets _ -> 0x10000u

    member x.Version =
       match x with
       | None -> 65536u
       | Bias _ -> 65536u
       | Lod _ -> 65536u
       | Grad _ -> 65536u
       | ConstOffset _ -> 65536u
       | Offset _ -> 65536u
       | ConstOffsets _ -> 65536u
       | Sample _ -> 65536u
       | MinLod _ -> 65536u
       | MakeTexelAvailable _ -> 66816u
       | MakeTexelVisible _ -> 66816u
       | NonPrivateTexel -> 66816u
       | VolatileTexel -> 66816u
       | SignExtend -> 66560u
       | ZeroExtend -> 66560u
       | Nontemporal -> 67072u
       | Offsets _ -> 65536u


type FPFastMathMode =
   | None = 0x0000u
   | NotNaN = 0x0001u
   | NotInf = 0x0002u
   | NSZ = 0x0004u
   | AllowRecip = 0x0008u
   | Fast = 0x0010u
   | AllowContract = 0x10000u
   | AllowReassoc = 0x20000u
   | AllowTransform = 0x40000u

module FPFastMathMode =

    let GetVersion x =
       match x with
       | FPFastMathMode.None -> 65536u
       | FPFastMathMode.NotNaN -> 65536u
       | FPFastMathMode.NotInf -> 65536u
       | FPFastMathMode.NSZ -> 65536u
       | FPFastMathMode.AllowRecip -> 65536u
       | FPFastMathMode.Fast -> 65536u
       | FPFastMathMode.AllowContract -> 65536u
       | FPFastMathMode.AllowReassoc -> 65536u
       | FPFastMathMode.AllowTransform -> 65536u

type SelectionControl =
   | None = 0x0000u
   | Flatten = 0x0001u
   | DontFlatten = 0x0002u

module SelectionControl =

    let GetVersion x =
       match x with
       | SelectionControl.None -> 65536u
       | SelectionControl.Flatten -> 65536u
       | SelectionControl.DontFlatten -> 65536u

[<RequireQualifiedAccess>]
type LoopControl =
    | None
    | Unroll
    | DontUnroll
    | DependencyInfinite
    | DependencyLength of LiteralInteger
    | MinIterations of LiteralInteger
    | MaxIterations of LiteralInteger
    | IterationMultiple of LiteralInteger
    | PeelCount of LiteralInteger
    | PartialCount of LiteralInteger
    | InitiationIntervalINTEL of LiteralInteger
    | MaxConcurrencyINTEL of LiteralInteger
    | DependencyArrayINTEL of LiteralInteger
    | PipelineEnableINTEL of LiteralInteger
    | LoopCoalesceINTEL of LiteralInteger
    | MaxInterleavingINTEL of LiteralInteger
    | SpeculatedIterationsINTEL of LiteralInteger
    | NoFusionINTEL
    | LoopCountINTEL of LiteralInteger
    | MaxReinvocationDelayINTEL of LiteralInteger

    member x.Value =
       match x with
       | None -> 0x0000u
       | Unroll -> 0x0001u
       | DontUnroll -> 0x0002u
       | DependencyInfinite -> 0x0004u
       | DependencyLength _ -> 0x0008u
       | MinIterations _ -> 0x0010u
       | MaxIterations _ -> 0x0020u
       | IterationMultiple _ -> 0x0040u
       | PeelCount _ -> 0x0080u
       | PartialCount _ -> 0x0100u
       | InitiationIntervalINTEL _ -> 0x10000u
       | MaxConcurrencyINTEL _ -> 0x20000u
       | DependencyArrayINTEL _ -> 0x40000u
       | PipelineEnableINTEL _ -> 0x80000u
       | LoopCoalesceINTEL _ -> 0x100000u
       | MaxInterleavingINTEL _ -> 0x200000u
       | SpeculatedIterationsINTEL _ -> 0x400000u
       | NoFusionINTEL -> 0x800000u
       | LoopCountINTEL _ -> 0x1000000u
       | MaxReinvocationDelayINTEL _ -> 0x2000000u

    member x.Version =
       match x with
       | None -> 65536u
       | Unroll -> 65536u
       | DontUnroll -> 65536u
       | DependencyInfinite -> 65792u
       | DependencyLength _ -> 65792u
       | MinIterations _ -> 66560u
       | MaxIterations _ -> 66560u
       | IterationMultiple _ -> 66560u
       | PeelCount _ -> 66560u
       | PartialCount _ -> 66560u
       | InitiationIntervalINTEL _ -> 65536u
       | MaxConcurrencyINTEL _ -> 65536u
       | DependencyArrayINTEL _ -> 65536u
       | PipelineEnableINTEL _ -> 65536u
       | LoopCoalesceINTEL _ -> 65536u
       | MaxInterleavingINTEL _ -> 65536u
       | SpeculatedIterationsINTEL _ -> 65536u
       | NoFusionINTEL -> 65536u
       | LoopCountINTEL _ -> 65536u
       | MaxReinvocationDelayINTEL _ -> 65536u


type FunctionControl =
   | None = 0x0000u
   | Inline = 0x0001u
   | DontInline = 0x0002u
   | Pure = 0x0004u
   | Const = 0x0008u
   | OptNoneEXT = 0x10000u

module FunctionControl =

    let GetVersion x =
       match x with
       | FunctionControl.None -> 65536u
       | FunctionControl.Inline -> 65536u
       | FunctionControl.DontInline -> 65536u
       | FunctionControl.Pure -> 65536u
       | FunctionControl.Const -> 65536u
       | FunctionControl.OptNoneEXT -> 65536u

type MemorySemantics =
   | Relaxed = 0x0000u
   | Acquire = 0x0002u
   | Release = 0x0004u
   | AcquireRelease = 0x0008u
   | SequentiallyConsistent = 0x0010u
   | UniformMemory = 0x0040u
   | SubgroupMemory = 0x0080u
   | WorkgroupMemory = 0x0100u
   | CrossWorkgroupMemory = 0x0200u
   | AtomicCounterMemory = 0x0400u
   | ImageMemory = 0x0800u
   | OutputMemory = 0x1000u
   | MakeAvailable = 0x2000u
   | MakeVisible = 0x4000u
   | Volatile = 0x8000u

module MemorySemantics =

    let GetVersion x =
       match x with
       | MemorySemantics.Relaxed -> 65536u
       | MemorySemantics.Acquire -> 65536u
       | MemorySemantics.Release -> 65536u
       | MemorySemantics.AcquireRelease -> 65536u
       | MemorySemantics.SequentiallyConsistent -> 65536u
       | MemorySemantics.UniformMemory -> 65536u
       | MemorySemantics.SubgroupMemory -> 65536u
       | MemorySemantics.WorkgroupMemory -> 65536u
       | MemorySemantics.CrossWorkgroupMemory -> 65536u
       | MemorySemantics.AtomicCounterMemory -> 65536u
       | MemorySemantics.ImageMemory -> 65536u
       | MemorySemantics.OutputMemory -> 66816u
       | MemorySemantics.MakeAvailable -> 66816u
       | MemorySemantics.MakeVisible -> 66816u
       | MemorySemantics.Volatile -> 66816u

[<RequireQualifiedAccess>]
type MemoryAccess =
    | None
    | Volatile
    | Aligned of LiteralInteger
    | Nontemporal
    | MakePointerAvailable of IdScope
    | MakePointerVisible of IdScope
    | NonPrivatePointer
    | AliasScopeINTELMask of IdRef
    | NoAliasINTELMask of IdRef

    member x.Value =
       match x with
       | None -> 0x0000u
       | Volatile -> 0x0001u
       | Aligned _ -> 0x0002u
       | Nontemporal -> 0x0004u
       | MakePointerAvailable _ -> 0x0008u
       | MakePointerVisible _ -> 0x0010u
       | NonPrivatePointer -> 0x0020u
       | AliasScopeINTELMask _ -> 0x10000u
       | NoAliasINTELMask _ -> 0x20000u

    member x.Version =
       match x with
       | None -> 65536u
       | Volatile -> 65536u
       | Aligned _ -> 65536u
       | Nontemporal -> 65536u
       | MakePointerAvailable _ -> 66816u
       | MakePointerVisible _ -> 66816u
       | NonPrivatePointer -> 66816u
       | AliasScopeINTELMask _ -> 65536u
       | NoAliasINTELMask _ -> 65536u


type KernelProfilingInfo =
   | None = 0x0000u
   | CmdExecTime = 0x0001u

module KernelProfilingInfo =

    let GetVersion x =
       match x with
       | KernelProfilingInfo.None -> 65536u
       | KernelProfilingInfo.CmdExecTime -> 65536u

type RayFlags =
   | NoneKHR = 0x0000u
   | OpaqueKHR = 0x0001u
   | NoOpaqueKHR = 0x0002u
   | TerminateOnFirstHitKHR = 0x0004u
   | SkipClosestHitShaderKHR = 0x0008u
   | CullBackFacingTrianglesKHR = 0x0010u
   | CullFrontFacingTrianglesKHR = 0x0020u
   | CullOpaqueKHR = 0x0040u
   | CullNoOpaqueKHR = 0x0080u
   | SkipTrianglesKHR = 0x0100u
   | SkipAABBsKHR = 0x0200u
   | ForceOpacityMicromap2StateEXT = 0x0400u

module RayFlags =

    let GetVersion x =
       match x with
       | RayFlags.NoneKHR -> 65536u
       | RayFlags.OpaqueKHR -> 65536u
       | RayFlags.NoOpaqueKHR -> 65536u
       | RayFlags.TerminateOnFirstHitKHR -> 65536u
       | RayFlags.SkipClosestHitShaderKHR -> 65536u
       | RayFlags.CullBackFacingTrianglesKHR -> 65536u
       | RayFlags.CullFrontFacingTrianglesKHR -> 65536u
       | RayFlags.CullOpaqueKHR -> 65536u
       | RayFlags.CullNoOpaqueKHR -> 65536u
       | RayFlags.SkipTrianglesKHR -> 65536u
       | RayFlags.SkipAABBsKHR -> 65536u
       | RayFlags.ForceOpacityMicromap2StateEXT -> 65536u

type FragmentShadingRate =
   | Vertical2Pixels = 0x0001u
   | Vertical4Pixels = 0x0002u
   | Horizontal2Pixels = 0x0004u
   | Horizontal4Pixels = 0x0008u

module FragmentShadingRate =

    let GetVersion x =
       match x with
       | FragmentShadingRate.Vertical2Pixels -> 65536u
       | FragmentShadingRate.Vertical4Pixels -> 65536u
       | FragmentShadingRate.Horizontal2Pixels -> 65536u
       | FragmentShadingRate.Horizontal4Pixels -> 65536u

type RawAccessChainOperands =
   | None = 0x0000u
   | RobustnessPerComponentNV = 0x0001u
   | RobustnessPerElementNV = 0x0002u

module RawAccessChainOperands =

    let GetVersion x =
       match x with
       | RawAccessChainOperands.None -> 65536u
       | RawAccessChainOperands.RobustnessPerComponentNV -> 65536u
       | RawAccessChainOperands.RobustnessPerElementNV -> 65536u

type SourceLanguage =
   | Unknown = 0u
   | ESSL = 1u
   | GLSL = 2u
   | OpenCL_C = 3u
   | OpenCL_CPP = 4u
   | HLSL = 5u
   | CPP_for_OpenCL = 6u
   | SYCL = 7u
   | HERO_C = 8u
   | NZSL = 9u
   | WGSL = 10u
   | Slang = 11u
   | Zig = 12u

module SourceLanguage =

    let GetVersion x =
       match x with
       | SourceLanguage.Unknown -> 65536u
       | SourceLanguage.ESSL -> 65536u
       | SourceLanguage.GLSL -> 65536u
       | SourceLanguage.OpenCL_C -> 65536u
       | SourceLanguage.OpenCL_CPP -> 65536u
       | SourceLanguage.HLSL -> 65536u
       | SourceLanguage.CPP_for_OpenCL -> 65536u
       | SourceLanguage.SYCL -> 65536u
       | SourceLanguage.HERO_C -> 65536u
       | SourceLanguage.NZSL -> 65536u
       | SourceLanguage.WGSL -> 65536u
       | SourceLanguage.Slang -> 65536u
       | SourceLanguage.Zig -> 65536u

type ExecutionModel =
   | Vertex = 0u
   | TessellationControl = 1u
   | TessellationEvaluation = 2u
   | Geometry = 3u
   | Fragment = 4u
   | GLCompute = 5u
   | Kernel = 6u
   | TaskNV = 5267u
   | MeshNV = 5268u
   | RayGenerationKHR = 5313u
   | IntersectionKHR = 5314u
   | AnyHitKHR = 5315u
   | ClosestHitKHR = 5316u
   | MissKHR = 5317u
   | CallableKHR = 5318u
   | TaskEXT = 5364u
   | MeshEXT = 5365u

module ExecutionModel =

    let GetVersion x =
       match x with
       | ExecutionModel.Vertex -> 65536u
       | ExecutionModel.TessellationControl -> 65536u
       | ExecutionModel.TessellationEvaluation -> 65536u
       | ExecutionModel.Geometry -> 65536u
       | ExecutionModel.Fragment -> 65536u
       | ExecutionModel.GLCompute -> 65536u
       | ExecutionModel.Kernel -> 65536u
       | ExecutionModel.TaskNV -> 65536u
       | ExecutionModel.MeshNV -> 65536u
       | ExecutionModel.RayGenerationKHR -> 65536u
       | ExecutionModel.IntersectionKHR -> 65536u
       | ExecutionModel.AnyHitKHR -> 65536u
       | ExecutionModel.ClosestHitKHR -> 65536u
       | ExecutionModel.MissKHR -> 65536u
       | ExecutionModel.CallableKHR -> 65536u
       | ExecutionModel.TaskEXT -> 65536u
       | ExecutionModel.MeshEXT -> 65536u

type AddressingModel =
   | Logical = 0u
   | Physical32 = 1u
   | Physical64 = 2u
   | PhysicalStorageBuffer64 = 5348u

module AddressingModel =

    let GetVersion x =
       match x with
       | AddressingModel.Logical -> 65536u
       | AddressingModel.Physical32 -> 65536u
       | AddressingModel.Physical64 -> 65536u
       | AddressingModel.PhysicalStorageBuffer64 -> 66816u

type MemoryModel =
   | Simple = 0u
   | GLSL450 = 1u
   | OpenCL = 2u
   | Vulkan = 3u

module MemoryModel =

    let GetVersion x =
       match x with
       | MemoryModel.Simple -> 65536u
       | MemoryModel.GLSL450 -> 65536u
       | MemoryModel.OpenCL -> 65536u
       | MemoryModel.Vulkan -> 66816u

[<RequireQualifiedAccess>]
type ExecutionMode =
    | Invocations of NumberofInvocationinvocations: LiteralInteger
    | SpacingEqual
    | SpacingFractionalEven
    | SpacingFractionalOdd
    | VertexOrderCw
    | VertexOrderCcw
    | PixelCenterInteger
    | OriginUpperLeft
    | OriginLowerLeft
    | EarlyFragmentTests
    | PointMode
    | Xfb
    | DepthReplacing
    | DepthGreater
    | DepthLess
    | DepthUnchanged
    | LocalSize of xsize: LiteralInteger * ysize: LiteralInteger * zsize: LiteralInteger
    | LocalSizeHint of xsize: LiteralInteger * ysize: LiteralInteger * zsize: LiteralInteger
    | InputPoints
    | InputLines
    | InputLinesAdjacency
    | Triangles
    | InputTrianglesAdjacency
    | Quads
    | Isolines
    | OutputVertices of Vertexcount: LiteralInteger
    | OutputPoints
    | OutputLineStrip
    | OutputTriangleStrip
    | VecTypeHint of Vectortype: LiteralInteger
    | ContractionOff
    | Initializer
    | Finalizer
    | SubgroupSize of SubgroupSize: LiteralInteger
    | SubgroupsPerWorkgroup of SubgroupsPerWorkgroup: LiteralInteger
    | SubgroupsPerWorkgroupId of SubgroupsPerWorkgroup: IdRef
    | LocalSizeId of xsize: IdRef * ysize: IdRef * zsize: IdRef
    | LocalSizeHintId of xsizehint: IdRef * ysizehint: IdRef * zsizehint: IdRef
    | NonCoherentColorAttachmentReadEXT
    | NonCoherentDepthAttachmentReadEXT
    | NonCoherentStencilAttachmentReadEXT
    | SubgroupUniformControlFlowKHR
    | PostDepthCoverage
    | DenormPreserve of TargetWidth: LiteralInteger
    | DenormFlushToZero of TargetWidth: LiteralInteger
    | SignedZeroInfNanPreserve of TargetWidth: LiteralInteger
    | RoundingModeRTE of TargetWidth: LiteralInteger
    | RoundingModeRTZ of TargetWidth: LiteralInteger
    | EarlyAndLateFragmentTestsAMD
    | StencilRefReplacingEXT
    | CoalescingAMDX
    | IsApiEntryAMDX of IsEntry: IdRef
    | MaxNodeRecursionAMDX of Numberofrecursions: IdRef
    | StaticNumWorkgroupsAMDX of xsize: IdRef * ysize: IdRef * zsize: IdRef
    | ShaderIndexAMDX of ShaderIndex: IdRef
    | MaxNumWorkgroupsAMDX of xsize: IdRef * ysize: IdRef * zsize: IdRef
    | StencilRefUnchangedFrontAMD
    | StencilRefGreaterFrontAMD
    | StencilRefLessFrontAMD
    | StencilRefUnchangedBackAMD
    | StencilRefGreaterBackAMD
    | StencilRefLessBackAMD
    | QuadDerivativesKHR
    | RequireFullQuadsKHR
    | SharesInputWithAMDX of NodeName: IdRef * ShaderIndex: IdRef
    | OutputLinesEXT
    | OutputPrimitivesEXT of Primitivecount: LiteralInteger
    | DerivativeGroupQuadsKHR
    | DerivativeGroupLinearKHR
    | OutputTrianglesEXT
    | PixelInterlockOrderedEXT
    | PixelInterlockUnorderedEXT
    | SampleInterlockOrderedEXT
    | SampleInterlockUnorderedEXT
    | ShadingRateInterlockOrderedEXT
    | ShadingRateInterlockUnorderedEXT
    | SharedLocalMemorySizeINTEL of Size: LiteralInteger
    | RoundingModeRTPINTEL of TargetWidth: LiteralInteger
    | RoundingModeRTNINTEL of TargetWidth: LiteralInteger
    | FloatingPointModeALTINTEL of TargetWidth: LiteralInteger
    | FloatingPointModeIEEEINTEL of TargetWidth: LiteralInteger
    | MaxWorkgroupSizeINTEL of max_x_size: LiteralInteger * max_y_size: LiteralInteger * max_z_size: LiteralInteger
    | MaxWorkDimINTEL of max_dimensions: LiteralInteger
    | NoGlobalOffsetINTEL
    | NumSIMDWorkitemsINTEL of vector_width: LiteralInteger
    | SchedulerTargetFmaxMhzINTEL of target_fmax: LiteralInteger
    | MaximallyReconvergesKHR
    | FPFastMathDefault of TargetType: IdRef * FastMathMode: IdRef
    | StreamingInterfaceINTEL of StallFreeReturn: LiteralInteger
    | RegisterMapInterfaceINTEL of WaitForDoneWrite: LiteralInteger
    | NamedBarrierCountINTEL of BarrierCount: LiteralInteger
    | MaximumRegistersINTEL of NumberofRegisters: LiteralInteger
    | MaximumRegistersIdINTEL of NumberofRegisters: IdRef
    | NamedMaximumRegistersINTEL of NamedMaximumNumberofRegisters: NamedMaximumNumberOfRegisters

    member x.Value =
       match x with
       | Invocations _ -> 0u
       | SpacingEqual -> 1u
       | SpacingFractionalEven -> 2u
       | SpacingFractionalOdd -> 3u
       | VertexOrderCw -> 4u
       | VertexOrderCcw -> 5u
       | PixelCenterInteger -> 6u
       | OriginUpperLeft -> 7u
       | OriginLowerLeft -> 8u
       | EarlyFragmentTests -> 9u
       | PointMode -> 10u
       | Xfb -> 11u
       | DepthReplacing -> 12u
       | DepthGreater -> 14u
       | DepthLess -> 15u
       | DepthUnchanged -> 16u
       | LocalSize _ -> 17u
       | LocalSizeHint _ -> 18u
       | InputPoints -> 19u
       | InputLines -> 20u
       | InputLinesAdjacency -> 21u
       | Triangles -> 22u
       | InputTrianglesAdjacency -> 23u
       | Quads -> 24u
       | Isolines -> 25u
       | OutputVertices _ -> 26u
       | OutputPoints -> 27u
       | OutputLineStrip -> 28u
       | OutputTriangleStrip -> 29u
       | VecTypeHint _ -> 30u
       | ContractionOff -> 31u
       | Initializer -> 33u
       | Finalizer -> 34u
       | SubgroupSize _ -> 35u
       | SubgroupsPerWorkgroup _ -> 36u
       | SubgroupsPerWorkgroupId _ -> 37u
       | LocalSizeId _ -> 38u
       | LocalSizeHintId _ -> 39u
       | NonCoherentColorAttachmentReadEXT -> 4169u
       | NonCoherentDepthAttachmentReadEXT -> 4170u
       | NonCoherentStencilAttachmentReadEXT -> 4171u
       | SubgroupUniformControlFlowKHR -> 4421u
       | PostDepthCoverage -> 4446u
       | DenormPreserve _ -> 4459u
       | DenormFlushToZero _ -> 4460u
       | SignedZeroInfNanPreserve _ -> 4461u
       | RoundingModeRTE _ -> 4462u
       | RoundingModeRTZ _ -> 4463u
       | EarlyAndLateFragmentTestsAMD -> 5017u
       | StencilRefReplacingEXT -> 5027u
       | CoalescingAMDX -> 5069u
       | IsApiEntryAMDX _ -> 5070u
       | MaxNodeRecursionAMDX _ -> 5071u
       | StaticNumWorkgroupsAMDX _ -> 5072u
       | ShaderIndexAMDX _ -> 5073u
       | MaxNumWorkgroupsAMDX _ -> 5077u
       | StencilRefUnchangedFrontAMD -> 5079u
       | StencilRefGreaterFrontAMD -> 5080u
       | StencilRefLessFrontAMD -> 5081u
       | StencilRefUnchangedBackAMD -> 5082u
       | StencilRefGreaterBackAMD -> 5083u
       | StencilRefLessBackAMD -> 5084u
       | QuadDerivativesKHR -> 5088u
       | RequireFullQuadsKHR -> 5089u
       | SharesInputWithAMDX _ -> 5102u
       | OutputLinesEXT -> 5269u
       | OutputPrimitivesEXT _ -> 5270u
       | DerivativeGroupQuadsKHR -> 5289u
       | DerivativeGroupLinearKHR -> 5290u
       | OutputTrianglesEXT -> 5298u
       | PixelInterlockOrderedEXT -> 5366u
       | PixelInterlockUnorderedEXT -> 5367u
       | SampleInterlockOrderedEXT -> 5368u
       | SampleInterlockUnorderedEXT -> 5369u
       | ShadingRateInterlockOrderedEXT -> 5370u
       | ShadingRateInterlockUnorderedEXT -> 5371u
       | SharedLocalMemorySizeINTEL _ -> 5618u
       | RoundingModeRTPINTEL _ -> 5620u
       | RoundingModeRTNINTEL _ -> 5621u
       | FloatingPointModeALTINTEL _ -> 5622u
       | FloatingPointModeIEEEINTEL _ -> 5623u
       | MaxWorkgroupSizeINTEL _ -> 5893u
       | MaxWorkDimINTEL _ -> 5894u
       | NoGlobalOffsetINTEL -> 5895u
       | NumSIMDWorkitemsINTEL _ -> 5896u
       | SchedulerTargetFmaxMhzINTEL _ -> 5903u
       | MaximallyReconvergesKHR -> 6023u
       | FPFastMathDefault _ -> 6028u
       | StreamingInterfaceINTEL _ -> 6154u
       | RegisterMapInterfaceINTEL _ -> 6160u
       | NamedBarrierCountINTEL _ -> 6417u
       | MaximumRegistersINTEL _ -> 6461u
       | MaximumRegistersIdINTEL _ -> 6462u
       | NamedMaximumRegistersINTEL _ -> 6463u

    member x.Version =
       match x with
       | Invocations _ -> 65536u
       | SpacingEqual -> 65536u
       | SpacingFractionalEven -> 65536u
       | SpacingFractionalOdd -> 65536u
       | VertexOrderCw -> 65536u
       | VertexOrderCcw -> 65536u
       | PixelCenterInteger -> 65536u
       | OriginUpperLeft -> 65536u
       | OriginLowerLeft -> 65536u
       | EarlyFragmentTests -> 65536u
       | PointMode -> 65536u
       | Xfb -> 65536u
       | DepthReplacing -> 65536u
       | DepthGreater -> 65536u
       | DepthLess -> 65536u
       | DepthUnchanged -> 65536u
       | LocalSize _ -> 65536u
       | LocalSizeHint _ -> 65536u
       | InputPoints -> 65536u
       | InputLines -> 65536u
       | InputLinesAdjacency -> 65536u
       | Triangles -> 65536u
       | InputTrianglesAdjacency -> 65536u
       | Quads -> 65536u
       | Isolines -> 65536u
       | OutputVertices _ -> 65536u
       | OutputPoints -> 65536u
       | OutputLineStrip -> 65536u
       | OutputTriangleStrip -> 65536u
       | VecTypeHint _ -> 65536u
       | ContractionOff -> 65536u
       | Initializer -> 65792u
       | Finalizer -> 65792u
       | SubgroupSize _ -> 65792u
       | SubgroupsPerWorkgroup _ -> 65792u
       | SubgroupsPerWorkgroupId _ -> 66048u
       | LocalSizeId _ -> 66048u
       | LocalSizeHintId _ -> 66048u
       | NonCoherentColorAttachmentReadEXT -> 65536u
       | NonCoherentDepthAttachmentReadEXT -> 65536u
       | NonCoherentStencilAttachmentReadEXT -> 65536u
       | SubgroupUniformControlFlowKHR -> 65536u
       | PostDepthCoverage -> 65536u
       | DenormPreserve _ -> 66560u
       | DenormFlushToZero _ -> 66560u
       | SignedZeroInfNanPreserve _ -> 66560u
       | RoundingModeRTE _ -> 66560u
       | RoundingModeRTZ _ -> 66560u
       | EarlyAndLateFragmentTestsAMD -> 65536u
       | StencilRefReplacingEXT -> 65536u
       | CoalescingAMDX -> 65536u
       | IsApiEntryAMDX _ -> 65536u
       | MaxNodeRecursionAMDX _ -> 65536u
       | StaticNumWorkgroupsAMDX _ -> 65536u
       | ShaderIndexAMDX _ -> 65536u
       | MaxNumWorkgroupsAMDX _ -> 65536u
       | StencilRefUnchangedFrontAMD -> 65536u
       | StencilRefGreaterFrontAMD -> 65536u
       | StencilRefLessFrontAMD -> 65536u
       | StencilRefUnchangedBackAMD -> 65536u
       | StencilRefGreaterBackAMD -> 65536u
       | StencilRefLessBackAMD -> 65536u
       | QuadDerivativesKHR -> 65536u
       | RequireFullQuadsKHR -> 65536u
       | SharesInputWithAMDX _ -> 65536u
       | OutputLinesEXT -> 65536u
       | OutputPrimitivesEXT _ -> 65536u
       | DerivativeGroupQuadsKHR -> 65536u
       | DerivativeGroupLinearKHR -> 65536u
       | OutputTrianglesEXT -> 65536u
       | PixelInterlockOrderedEXT -> 65536u
       | PixelInterlockUnorderedEXT -> 65536u
       | SampleInterlockOrderedEXT -> 65536u
       | SampleInterlockUnorderedEXT -> 65536u
       | ShadingRateInterlockOrderedEXT -> 65536u
       | ShadingRateInterlockUnorderedEXT -> 65536u
       | SharedLocalMemorySizeINTEL _ -> 65536u
       | RoundingModeRTPINTEL _ -> 65536u
       | RoundingModeRTNINTEL _ -> 65536u
       | FloatingPointModeALTINTEL _ -> 65536u
       | FloatingPointModeIEEEINTEL _ -> 65536u
       | MaxWorkgroupSizeINTEL _ -> 65536u
       | MaxWorkDimINTEL _ -> 65536u
       | NoGlobalOffsetINTEL -> 65536u
       | NumSIMDWorkitemsINTEL _ -> 65536u
       | SchedulerTargetFmaxMhzINTEL _ -> 65536u
       | MaximallyReconvergesKHR -> 65536u
       | FPFastMathDefault _ -> 65536u
       | StreamingInterfaceINTEL _ -> 65536u
       | RegisterMapInterfaceINTEL _ -> 65536u
       | NamedBarrierCountINTEL _ -> 65536u
       | MaximumRegistersINTEL _ -> 65536u
       | MaximumRegistersIdINTEL _ -> 65536u
       | NamedMaximumRegistersINTEL _ -> 65536u


type StorageClass =
   | UniformConstant = 0u
   | Input = 1u
   | Uniform = 2u
   | Output = 3u
   | Workgroup = 4u
   | CrossWorkgroup = 5u
   | Private = 6u
   | Function = 7u
   | Generic = 8u
   | PushConstant = 9u
   | AtomicCounter = 10u
   | Image = 11u
   | StorageBuffer = 12u
   | TileImageEXT = 4172u
   | NodePayloadAMDX = 5068u
   | CallableDataKHR = 5328u
   | IncomingCallableDataKHR = 5329u
   | RayPayloadKHR = 5338u
   | HitAttributeKHR = 5339u
   | IncomingRayPayloadKHR = 5342u
   | ShaderRecordBufferKHR = 5343u
   | PhysicalStorageBuffer = 5349u
   | HitObjectAttributeNV = 5385u
   | TaskPayloadWorkgroupEXT = 5402u
   | CodeSectionINTEL = 5605u
   | DeviceOnlyINTEL = 5936u
   | HostOnlyINTEL = 5937u

module StorageClass =

    let GetVersion x =
       match x with
       | StorageClass.UniformConstant -> 65536u
       | StorageClass.Input -> 65536u
       | StorageClass.Uniform -> 65536u
       | StorageClass.Output -> 65536u
       | StorageClass.Workgroup -> 65536u
       | StorageClass.CrossWorkgroup -> 65536u
       | StorageClass.Private -> 65536u
       | StorageClass.Function -> 65536u
       | StorageClass.Generic -> 65536u
       | StorageClass.PushConstant -> 65536u
       | StorageClass.AtomicCounter -> 65536u
       | StorageClass.Image -> 65536u
       | StorageClass.StorageBuffer -> 66304u
       | StorageClass.TileImageEXT -> 65536u
       | StorageClass.NodePayloadAMDX -> 65536u
       | StorageClass.CallableDataKHR -> 65536u
       | StorageClass.IncomingCallableDataKHR -> 65536u
       | StorageClass.RayPayloadKHR -> 65536u
       | StorageClass.HitAttributeKHR -> 65536u
       | StorageClass.IncomingRayPayloadKHR -> 65536u
       | StorageClass.ShaderRecordBufferKHR -> 65536u
       | StorageClass.PhysicalStorageBuffer -> 66816u
       | StorageClass.HitObjectAttributeNV -> 65536u
       | StorageClass.TaskPayloadWorkgroupEXT -> 66560u
       | StorageClass.CodeSectionINTEL -> 65536u
       | StorageClass.DeviceOnlyINTEL -> 65536u
       | StorageClass.HostOnlyINTEL -> 65536u

type Dim =
   | One = 0u
   | Two = 1u
   | Three = 2u
   | Cube = 3u
   | Rect = 4u
   | Buffer = 5u
   | SubpassData = 6u
   | TileImageDataEXT = 4173u

module Dim =

    let GetVersion x =
       match x with
       | Dim.One -> 65536u
       | Dim.Two -> 65536u
       | Dim.Three -> 65536u
       | Dim.Cube -> 65536u
       | Dim.Rect -> 65536u
       | Dim.Buffer -> 65536u
       | Dim.SubpassData -> 65536u
       | Dim.TileImageDataEXT -> 65536u

type SamplerAddressingMode =
   | None = 0u
   | ClampToEdge = 1u
   | Clamp = 2u
   | Repeat = 3u
   | RepeatMirrored = 4u

module SamplerAddressingMode =

    let GetVersion x =
       match x with
       | SamplerAddressingMode.None -> 65536u
       | SamplerAddressingMode.ClampToEdge -> 65536u
       | SamplerAddressingMode.Clamp -> 65536u
       | SamplerAddressingMode.Repeat -> 65536u
       | SamplerAddressingMode.RepeatMirrored -> 65536u

type SamplerFilterMode =
   | Nearest = 0u
   | Linear = 1u

module SamplerFilterMode =

    let GetVersion x =
       match x with
       | SamplerFilterMode.Nearest -> 65536u
       | SamplerFilterMode.Linear -> 65536u

type ImageFormat =
   | Unknown = 0u
   | Rgba32f = 1u
   | Rgba16f = 2u
   | R32f = 3u
   | Rgba8 = 4u
   | Rgba8Snorm = 5u
   | Rg32f = 6u
   | Rg16f = 7u
   | R11fG11fB10f = 8u
   | R16f = 9u
   | Rgba16 = 10u
   | Rgb10A2 = 11u
   | Rg16 = 12u
   | Rg8 = 13u
   | R16 = 14u
   | R8 = 15u
   | Rgba16Snorm = 16u
   | Rg16Snorm = 17u
   | Rg8Snorm = 18u
   | R16Snorm = 19u
   | R8Snorm = 20u
   | Rgba32i = 21u
   | Rgba16i = 22u
   | Rgba8i = 23u
   | R32i = 24u
   | Rg32i = 25u
   | Rg16i = 26u
   | Rg8i = 27u
   | R16i = 28u
   | R8i = 29u
   | Rgba32ui = 30u
   | Rgba16ui = 31u
   | Rgba8ui = 32u
   | R32ui = 33u
   | Rgb10a2ui = 34u
   | Rg32ui = 35u
   | Rg16ui = 36u
   | Rg8ui = 37u
   | R16ui = 38u
   | R8ui = 39u
   | R64ui = 40u
   | R64i = 41u

module ImageFormat =

    let GetVersion x =
       match x with
       | ImageFormat.Unknown -> 65536u
       | ImageFormat.Rgba32f -> 65536u
       | ImageFormat.Rgba16f -> 65536u
       | ImageFormat.R32f -> 65536u
       | ImageFormat.Rgba8 -> 65536u
       | ImageFormat.Rgba8Snorm -> 65536u
       | ImageFormat.Rg32f -> 65536u
       | ImageFormat.Rg16f -> 65536u
       | ImageFormat.R11fG11fB10f -> 65536u
       | ImageFormat.R16f -> 65536u
       | ImageFormat.Rgba16 -> 65536u
       | ImageFormat.Rgb10A2 -> 65536u
       | ImageFormat.Rg16 -> 65536u
       | ImageFormat.Rg8 -> 65536u
       | ImageFormat.R16 -> 65536u
       | ImageFormat.R8 -> 65536u
       | ImageFormat.Rgba16Snorm -> 65536u
       | ImageFormat.Rg16Snorm -> 65536u
       | ImageFormat.Rg8Snorm -> 65536u
       | ImageFormat.R16Snorm -> 65536u
       | ImageFormat.R8Snorm -> 65536u
       | ImageFormat.Rgba32i -> 65536u
       | ImageFormat.Rgba16i -> 65536u
       | ImageFormat.Rgba8i -> 65536u
       | ImageFormat.R32i -> 65536u
       | ImageFormat.Rg32i -> 65536u
       | ImageFormat.Rg16i -> 65536u
       | ImageFormat.Rg8i -> 65536u
       | ImageFormat.R16i -> 65536u
       | ImageFormat.R8i -> 65536u
       | ImageFormat.Rgba32ui -> 65536u
       | ImageFormat.Rgba16ui -> 65536u
       | ImageFormat.Rgba8ui -> 65536u
       | ImageFormat.R32ui -> 65536u
       | ImageFormat.Rgb10a2ui -> 65536u
       | ImageFormat.Rg32ui -> 65536u
       | ImageFormat.Rg16ui -> 65536u
       | ImageFormat.Rg8ui -> 65536u
       | ImageFormat.R16ui -> 65536u
       | ImageFormat.R8ui -> 65536u
       | ImageFormat.R64ui -> 65536u
       | ImageFormat.R64i -> 65536u

type ImageChannelOrder =
   | R = 0u
   | A = 1u
   | RG = 2u
   | RA = 3u
   | RGB = 4u
   | RGBA = 5u
   | BGRA = 6u
   | ARGB = 7u
   | Intensity = 8u
   | Luminance = 9u
   | Rx = 10u
   | RGx = 11u
   | RGBx = 12u
   | Depth = 13u
   | DepthStencil = 14u
   | sRGB = 15u
   | sRGBx = 16u
   | sRGBA = 17u
   | sBGRA = 18u
   | ABGR = 19u

module ImageChannelOrder =

    let GetVersion x =
       match x with
       | ImageChannelOrder.R -> 65536u
       | ImageChannelOrder.A -> 65536u
       | ImageChannelOrder.RG -> 65536u
       | ImageChannelOrder.RA -> 65536u
       | ImageChannelOrder.RGB -> 65536u
       | ImageChannelOrder.RGBA -> 65536u
       | ImageChannelOrder.BGRA -> 65536u
       | ImageChannelOrder.ARGB -> 65536u
       | ImageChannelOrder.Intensity -> 65536u
       | ImageChannelOrder.Luminance -> 65536u
       | ImageChannelOrder.Rx -> 65536u
       | ImageChannelOrder.RGx -> 65536u
       | ImageChannelOrder.RGBx -> 65536u
       | ImageChannelOrder.Depth -> 65536u
       | ImageChannelOrder.DepthStencil -> 65536u
       | ImageChannelOrder.sRGB -> 65536u
       | ImageChannelOrder.sRGBx -> 65536u
       | ImageChannelOrder.sRGBA -> 65536u
       | ImageChannelOrder.sBGRA -> 65536u
       | ImageChannelOrder.ABGR -> 65536u

type ImageChannelDataType =
   | SnormInt8 = 0u
   | SnormInt16 = 1u
   | UnormInt8 = 2u
   | UnormInt16 = 3u
   | UnormShort565 = 4u
   | UnormShort555 = 5u
   | UnormInt101010 = 6u
   | SignedInt8 = 7u
   | SignedInt16 = 8u
   | SignedInt32 = 9u
   | UnsignedInt8 = 10u
   | UnsignedInt16 = 11u
   | UnsignedInt32 = 12u
   | HalfFloat = 13u
   | Float = 14u
   | UnormInt24 = 15u
   | UnormInt101010_2 = 16u
   | UnsignedIntRaw10EXT = 19u
   | UnsignedIntRaw12EXT = 20u
   | UnormInt2_101010EXT = 21u

module ImageChannelDataType =

    let GetVersion x =
       match x with
       | ImageChannelDataType.SnormInt8 -> 65536u
       | ImageChannelDataType.SnormInt16 -> 65536u
       | ImageChannelDataType.UnormInt8 -> 65536u
       | ImageChannelDataType.UnormInt16 -> 65536u
       | ImageChannelDataType.UnormShort565 -> 65536u
       | ImageChannelDataType.UnormShort555 -> 65536u
       | ImageChannelDataType.UnormInt101010 -> 65536u
       | ImageChannelDataType.SignedInt8 -> 65536u
       | ImageChannelDataType.SignedInt16 -> 65536u
       | ImageChannelDataType.SignedInt32 -> 65536u
       | ImageChannelDataType.UnsignedInt8 -> 65536u
       | ImageChannelDataType.UnsignedInt16 -> 65536u
       | ImageChannelDataType.UnsignedInt32 -> 65536u
       | ImageChannelDataType.HalfFloat -> 65536u
       | ImageChannelDataType.Float -> 65536u
       | ImageChannelDataType.UnormInt24 -> 65536u
       | ImageChannelDataType.UnormInt101010_2 -> 65536u
       | ImageChannelDataType.UnsignedIntRaw10EXT -> 65536u
       | ImageChannelDataType.UnsignedIntRaw12EXT -> 65536u
       | ImageChannelDataType.UnormInt2_101010EXT -> 65536u

type FPRoundingMode =
   | RTE = 0u
   | RTZ = 1u
   | RTP = 2u
   | RTN = 3u

module FPRoundingMode =

    let GetVersion x =
       match x with
       | FPRoundingMode.RTE -> 65536u
       | FPRoundingMode.RTZ -> 65536u
       | FPRoundingMode.RTP -> 65536u
       | FPRoundingMode.RTN -> 65536u

type FPDenormMode =
   | Preserve = 0u
   | FlushToZero = 1u

module FPDenormMode =

    let GetVersion x =
       match x with
       | FPDenormMode.Preserve -> 65536u
       | FPDenormMode.FlushToZero -> 65536u

type QuantizationModes =
   | TRN = 0u
   | TRN_ZERO = 1u
   | RND = 2u
   | RND_ZERO = 3u
   | RND_INF = 4u
   | RND_MIN_INF = 5u
   | RND_CONV = 6u
   | RND_CONV_ODD = 7u

module QuantizationModes =

    let GetVersion x =
       match x with
       | QuantizationModes.TRN -> 65536u
       | QuantizationModes.TRN_ZERO -> 65536u
       | QuantizationModes.RND -> 65536u
       | QuantizationModes.RND_ZERO -> 65536u
       | QuantizationModes.RND_INF -> 65536u
       | QuantizationModes.RND_MIN_INF -> 65536u
       | QuantizationModes.RND_CONV -> 65536u
       | QuantizationModes.RND_CONV_ODD -> 65536u

type FPOperationMode =
   | IEEE = 0u
   | ALT = 1u

module FPOperationMode =

    let GetVersion x =
       match x with
       | FPOperationMode.IEEE -> 65536u
       | FPOperationMode.ALT -> 65536u

type OverflowModes =
   | WRAP = 0u
   | SAT = 1u
   | SAT_ZERO = 2u
   | SAT_SYM = 3u

module OverflowModes =

    let GetVersion x =
       match x with
       | OverflowModes.WRAP -> 65536u
       | OverflowModes.SAT -> 65536u
       | OverflowModes.SAT_ZERO -> 65536u
       | OverflowModes.SAT_SYM -> 65536u

type LinkageType =
   | Export = 0u
   | Import = 1u
   | LinkOnceODR = 2u

module LinkageType =

    let GetVersion x =
       match x with
       | LinkageType.Export -> 65536u
       | LinkageType.Import -> 65536u
       | LinkageType.LinkOnceODR -> 65536u

type AccessQualifier =
   | ReadOnly = 0u
   | WriteOnly = 1u
   | ReadWrite = 2u

module AccessQualifier =

    let GetVersion x =
       match x with
       | AccessQualifier.ReadOnly -> 65536u
       | AccessQualifier.WriteOnly -> 65536u
       | AccessQualifier.ReadWrite -> 65536u

type HostAccessQualifier =
   | NoneINTEL = 0u
   | ReadINTEL = 1u
   | WriteINTEL = 2u
   | ReadWriteINTEL = 3u

module HostAccessQualifier =

    let GetVersion x =
       match x with
       | HostAccessQualifier.NoneINTEL -> 65536u
       | HostAccessQualifier.ReadINTEL -> 65536u
       | HostAccessQualifier.WriteINTEL -> 65536u
       | HostAccessQualifier.ReadWriteINTEL -> 65536u

type FunctionParameterAttribute =
   | Zext = 0u
   | Sext = 1u
   | ByVal = 2u
   | Sret = 3u
   | NoAlias = 4u
   | NoCapture = 5u
   | NoWrite = 6u
   | NoReadWrite = 7u
   | RuntimeAlignedINTEL = 5940u

module FunctionParameterAttribute =

    let GetVersion x =
       match x with
       | FunctionParameterAttribute.Zext -> 65536u
       | FunctionParameterAttribute.Sext -> 65536u
       | FunctionParameterAttribute.ByVal -> 65536u
       | FunctionParameterAttribute.Sret -> 65536u
       | FunctionParameterAttribute.NoAlias -> 65536u
       | FunctionParameterAttribute.NoCapture -> 65536u
       | FunctionParameterAttribute.NoWrite -> 65536u
       | FunctionParameterAttribute.NoReadWrite -> 65536u
       | FunctionParameterAttribute.RuntimeAlignedINTEL -> 65536u

[<RequireQualifiedAccess>]
type Decoration =
    | RelaxedPrecision
    | SpecId of SpecializationConstantID: LiteralInteger
    | Block
    | BufferBlock
    | RowMajor
    | ColMajor
    | ArrayStride of ArrayStride: LiteralInteger
    | MatrixStride of MatrixStride: LiteralInteger
    | GLSLShared
    | GLSLPacked
    | CPacked
    | BuiltIn of BuiltIn
    | NoPerspective
    | Flat
    | Patch
    | Centroid
    | Sample
    | Invariant
    | Restrict
    | Aliased
    | Volatile
    | Constant
    | Coherent
    | NonWritable
    | NonReadable
    | Uniform
    | UniformId of Execution: IdScope
    | SaturatedConversion
    | Stream of StreamNumber: LiteralInteger
    | Location of Location: LiteralInteger
    | Component of Component: LiteralInteger
    | Index of Index: LiteralInteger
    | Binding of BindingPoint: LiteralInteger
    | DescriptorSet of DescriptorSet: LiteralInteger
    | Offset of ByteOffset: LiteralInteger
    | XfbBuffer of XFBBufferNumber: LiteralInteger
    | XfbStride of XFBStride: LiteralInteger
    | FuncParamAttr of FunctionParameterAttribute: FunctionParameterAttribute
    | FPRoundingMode of FloatingPointRoundingMode: FPRoundingMode
    | FPFastMathMode of FastMathMode: FPFastMathMode
    | LinkageAttributes of Name: LiteralString * LinkageType: LinkageType
    | NoContraction
    | InputAttachmentIndex of AttachmentIndex: LiteralInteger
    | Alignment of Alignment: LiteralInteger
    | MaxByteOffset of MaxByteOffset: LiteralInteger
    | AlignmentId of Alignment: IdRef
    | MaxByteOffsetId of MaxByteOffset: IdRef
    | NoSignedWrap
    | NoUnsignedWrap
    | WeightTextureQCOM
    | BlockMatchTextureQCOM
    | BlockMatchSamplerQCOM
    | ExplicitInterpAMD
    | NodeSharesPayloadLimitsWithAMDX of PayloadType: IdRef
    | NodeMaxPayloadsAMDX of Maxnumberofpayloads: IdRef
    | TrackFinishWritingAMDX
    | PayloadNodeNameAMDX of NodeName: IdRef
    | PayloadNodeBaseIndexAMDX of BaseIndex: IdRef
    | PayloadNodeSparseArrayAMDX
    | PayloadNodeArraySizeAMDX of ArraySize: IdRef
    | PayloadDispatchIndirectAMDX
    | OverrideCoverageNV
    | PassthroughNV
    | ViewportRelativeNV
    | SecondaryViewportRelativeNV of Offset: LiteralInteger
    | PerPrimitiveEXT
    | PerViewNV
    | PerTaskNV
    | PerVertexKHR
    | NonUniform
    | RestrictPointer
    | AliasedPointer
    | HitObjectShaderRecordBufferNV
    | BindlessSamplerNV
    | BindlessImageNV
    | BoundSamplerNV
    | BoundImageNV
    | SIMTCallINTEL of N: LiteralInteger
    | ReferencedIndirectlyINTEL
    | ClobberINTEL of Register: LiteralString
    | SideEffectsINTEL
    | VectorComputeVariableINTEL
    | FuncParamIOKindINTEL of Kind: LiteralInteger
    | VectorComputeFunctionINTEL
    | StackCallINTEL
    | GlobalVariableOffsetINTEL of Offset: LiteralInteger
    | CounterBuffer of CounterBuffer: IdRef
    | UserSemantic of Semantic: LiteralString
    | UserTypeGOOGLE of UserType: LiteralString
    | FunctionRoundingModeINTEL of TargetWidth: LiteralInteger * FPRoundingMode: FPRoundingMode
    | FunctionDenormModeINTEL of TargetWidth: LiteralInteger * FPDenormMode: FPDenormMode
    | RegisterINTEL
    | MemoryINTEL of MemoryType: LiteralString
    | NumbanksINTEL of Banks: LiteralInteger
    | BankwidthINTEL of BankWidth: LiteralInteger
    | MaxPrivateCopiesINTEL of MaximumCopies: LiteralInteger
    | SinglepumpINTEL
    | DoublepumpINTEL
    | MaxReplicatesINTEL of MaximumReplicates: LiteralInteger
    | SimpleDualPortINTEL
    | MergeINTEL of MergeKey: LiteralString * MergeType: LiteralString
    | BankBitsINTEL of BankBits: LiteralInteger
    | ForcePow2DepthINTEL of ForceKey: LiteralInteger
    | StridesizeINTEL of StrideSize: LiteralInteger
    | WordsizeINTEL of WordSize: LiteralInteger
    | TrueDualPortINTEL
    | BurstCoalesceINTEL
    | CacheSizeINTEL of CacheSizeinbytes: LiteralInteger
    | DontStaticallyCoalesceINTEL
    | PrefetchINTEL of PrefetcherSizeinbytes: LiteralInteger
    | StallEnableINTEL
    | FuseLoopsInFunctionINTEL
    | MathOpDSPModeINTEL of Mode: LiteralInteger * Propagate: LiteralInteger
    | AliasScopeINTEL of AliasingScopesList: IdRef
    | NoAliasINTEL of AliasingScopesList: IdRef
    | InitiationIntervalINTEL of Cycles: LiteralInteger
    | MaxConcurrencyINTEL of Invocations: LiteralInteger
    | PipelineEnableINTEL of Enable: LiteralInteger
    | BufferLocationINTEL of BufferLocationID: LiteralInteger
    | IOPipeStorageINTEL of IOPipeID: LiteralInteger
    | FunctionFloatingPointModeINTEL of TargetWidth: LiteralInteger * FPOperationMode: FPOperationMode
    | SingleElementVectorINTEL
    | VectorComputeCallableFunctionINTEL
    | MediaBlockIOINTEL
    | StallFreeINTEL
    | FPMaxErrorDecorationINTEL of MaxError: LiteralFloat
    | LatencyControlLabelINTEL of LatencyLabel: LiteralInteger
    | LatencyControlConstraintINTEL of RelativeTo: LiteralInteger * ControlType: LiteralInteger * RelativeCycle: LiteralInteger
    | ConduitKernelArgumentINTEL
    | RegisterMapKernelArgumentINTEL
    | MMHostInterfaceAddressWidthINTEL of AddressWidth: LiteralInteger
    | MMHostInterfaceDataWidthINTEL of DataWidth: LiteralInteger
    | MMHostInterfaceLatencyINTEL of Latency: LiteralInteger
    | MMHostInterfaceReadWriteModeINTEL of ReadWriteMode: AccessQualifier
    | MMHostInterfaceMaxBurstINTEL of MaxBurstCount: LiteralInteger
    | MMHostInterfaceWaitRequestINTEL of Waitrequest: LiteralInteger
    | StableKernelArgumentINTEL
    | HostAccessINTEL of Access: HostAccessQualifier * Name: LiteralString
    | InitModeINTEL of Trigger: InitializationModeQualifier
    | ImplementInRegisterMapINTEL of Value: LiteralInteger
    | CacheControlLoadINTEL of CacheLevel: LiteralInteger * CacheControl: LoadCacheControl
    | CacheControlStoreINTEL of CacheLevel: LiteralInteger * CacheControl: StoreCacheControl

    member x.Value =
       match x with
       | RelaxedPrecision -> 0u
       | SpecId _ -> 1u
       | Block -> 2u
       | BufferBlock -> 3u
       | RowMajor -> 4u
       | ColMajor -> 5u
       | ArrayStride _ -> 6u
       | MatrixStride _ -> 7u
       | GLSLShared -> 8u
       | GLSLPacked -> 9u
       | CPacked -> 10u
       | BuiltIn _ -> 11u
       | NoPerspective -> 13u
       | Flat -> 14u
       | Patch -> 15u
       | Centroid -> 16u
       | Sample -> 17u
       | Invariant -> 18u
       | Restrict -> 19u
       | Aliased -> 20u
       | Volatile -> 21u
       | Constant -> 22u
       | Coherent -> 23u
       | NonWritable -> 24u
       | NonReadable -> 25u
       | Uniform -> 26u
       | UniformId _ -> 27u
       | SaturatedConversion -> 28u
       | Stream _ -> 29u
       | Location _ -> 30u
       | Component _ -> 31u
       | Index _ -> 32u
       | Binding _ -> 33u
       | DescriptorSet _ -> 34u
       | Offset _ -> 35u
       | XfbBuffer _ -> 36u
       | XfbStride _ -> 37u
       | FuncParamAttr _ -> 38u
       | FPRoundingMode _ -> 39u
       | FPFastMathMode _ -> 40u
       | LinkageAttributes _ -> 41u
       | NoContraction -> 42u
       | InputAttachmentIndex _ -> 43u
       | Alignment _ -> 44u
       | MaxByteOffset _ -> 45u
       | AlignmentId _ -> 46u
       | MaxByteOffsetId _ -> 47u
       | NoSignedWrap -> 4469u
       | NoUnsignedWrap -> 4470u
       | WeightTextureQCOM -> 4487u
       | BlockMatchTextureQCOM -> 4488u
       | BlockMatchSamplerQCOM -> 4499u
       | ExplicitInterpAMD -> 4999u
       | NodeSharesPayloadLimitsWithAMDX _ -> 5019u
       | NodeMaxPayloadsAMDX _ -> 5020u
       | TrackFinishWritingAMDX -> 5078u
       | PayloadNodeNameAMDX _ -> 5091u
       | PayloadNodeBaseIndexAMDX _ -> 5098u
       | PayloadNodeSparseArrayAMDX -> 5099u
       | PayloadNodeArraySizeAMDX _ -> 5100u
       | PayloadDispatchIndirectAMDX -> 5105u
       | OverrideCoverageNV -> 5248u
       | PassthroughNV -> 5250u
       | ViewportRelativeNV -> 5252u
       | SecondaryViewportRelativeNV _ -> 5256u
       | PerPrimitiveEXT -> 5271u
       | PerViewNV -> 5272u
       | PerTaskNV -> 5273u
       | PerVertexKHR -> 5285u
       | NonUniform -> 5300u
       | RestrictPointer -> 5355u
       | AliasedPointer -> 5356u
       | HitObjectShaderRecordBufferNV -> 5386u
       | BindlessSamplerNV -> 5398u
       | BindlessImageNV -> 5399u
       | BoundSamplerNV -> 5400u
       | BoundImageNV -> 5401u
       | SIMTCallINTEL _ -> 5599u
       | ReferencedIndirectlyINTEL -> 5602u
       | ClobberINTEL _ -> 5607u
       | SideEffectsINTEL -> 5608u
       | VectorComputeVariableINTEL -> 5624u
       | FuncParamIOKindINTEL _ -> 5625u
       | VectorComputeFunctionINTEL -> 5626u
       | StackCallINTEL -> 5627u
       | GlobalVariableOffsetINTEL _ -> 5628u
       | CounterBuffer _ -> 5634u
       | UserSemantic _ -> 5635u
       | UserTypeGOOGLE _ -> 5636u
       | FunctionRoundingModeINTEL _ -> 5822u
       | FunctionDenormModeINTEL _ -> 5823u
       | RegisterINTEL -> 5825u
       | MemoryINTEL _ -> 5826u
       | NumbanksINTEL _ -> 5827u
       | BankwidthINTEL _ -> 5828u
       | MaxPrivateCopiesINTEL _ -> 5829u
       | SinglepumpINTEL -> 5830u
       | DoublepumpINTEL -> 5831u
       | MaxReplicatesINTEL _ -> 5832u
       | SimpleDualPortINTEL -> 5833u
       | MergeINTEL _ -> 5834u
       | BankBitsINTEL _ -> 5835u
       | ForcePow2DepthINTEL _ -> 5836u
       | StridesizeINTEL _ -> 5883u
       | WordsizeINTEL _ -> 5884u
       | TrueDualPortINTEL -> 5885u
       | BurstCoalesceINTEL -> 5899u
       | CacheSizeINTEL _ -> 5900u
       | DontStaticallyCoalesceINTEL -> 5901u
       | PrefetchINTEL _ -> 5902u
       | StallEnableINTEL -> 5905u
       | FuseLoopsInFunctionINTEL -> 5907u
       | MathOpDSPModeINTEL _ -> 5909u
       | AliasScopeINTEL _ -> 5914u
       | NoAliasINTEL _ -> 5915u
       | InitiationIntervalINTEL _ -> 5917u
       | MaxConcurrencyINTEL _ -> 5918u
       | PipelineEnableINTEL _ -> 5919u
       | BufferLocationINTEL _ -> 5921u
       | IOPipeStorageINTEL _ -> 5944u
       | FunctionFloatingPointModeINTEL _ -> 6080u
       | SingleElementVectorINTEL -> 6085u
       | VectorComputeCallableFunctionINTEL -> 6087u
       | MediaBlockIOINTEL -> 6140u
       | StallFreeINTEL -> 6151u
       | FPMaxErrorDecorationINTEL _ -> 6170u
       | LatencyControlLabelINTEL _ -> 6172u
       | LatencyControlConstraintINTEL _ -> 6173u
       | ConduitKernelArgumentINTEL -> 6175u
       | RegisterMapKernelArgumentINTEL -> 6176u
       | MMHostInterfaceAddressWidthINTEL _ -> 6177u
       | MMHostInterfaceDataWidthINTEL _ -> 6178u
       | MMHostInterfaceLatencyINTEL _ -> 6179u
       | MMHostInterfaceReadWriteModeINTEL _ -> 6180u
       | MMHostInterfaceMaxBurstINTEL _ -> 6181u
       | MMHostInterfaceWaitRequestINTEL _ -> 6182u
       | StableKernelArgumentINTEL -> 6183u
       | HostAccessINTEL _ -> 6188u
       | InitModeINTEL _ -> 6190u
       | ImplementInRegisterMapINTEL _ -> 6191u
       | CacheControlLoadINTEL _ -> 6442u
       | CacheControlStoreINTEL _ -> 6443u

    member x.Version =
       match x with
       | RelaxedPrecision -> 65536u
       | SpecId _ -> 65536u
       | Block -> 65536u
       | BufferBlock -> 65536u
       | RowMajor -> 65536u
       | ColMajor -> 65536u
       | ArrayStride _ -> 65536u
       | MatrixStride _ -> 65536u
       | GLSLShared -> 65536u
       | GLSLPacked -> 65536u
       | CPacked -> 65536u
       | BuiltIn _ -> 65536u
       | NoPerspective -> 65536u
       | Flat -> 65536u
       | Patch -> 65536u
       | Centroid -> 65536u
       | Sample -> 65536u
       | Invariant -> 65536u
       | Restrict -> 65536u
       | Aliased -> 65536u
       | Volatile -> 65536u
       | Constant -> 65536u
       | Coherent -> 65536u
       | NonWritable -> 65536u
       | NonReadable -> 65536u
       | Uniform -> 65536u
       | UniformId _ -> 66560u
       | SaturatedConversion -> 65536u
       | Stream _ -> 65536u
       | Location _ -> 65536u
       | Component _ -> 65536u
       | Index _ -> 65536u
       | Binding _ -> 65536u
       | DescriptorSet _ -> 65536u
       | Offset _ -> 65536u
       | XfbBuffer _ -> 65536u
       | XfbStride _ -> 65536u
       | FuncParamAttr _ -> 65536u
       | FPRoundingMode _ -> 65536u
       | FPFastMathMode _ -> 65536u
       | LinkageAttributes _ -> 65536u
       | NoContraction -> 65536u
       | InputAttachmentIndex _ -> 65536u
       | Alignment _ -> 65536u
       | MaxByteOffset _ -> 65792u
       | AlignmentId _ -> 66048u
       | MaxByteOffsetId _ -> 66048u
       | NoSignedWrap -> 66560u
       | NoUnsignedWrap -> 66560u
       | WeightTextureQCOM -> 65536u
       | BlockMatchTextureQCOM -> 65536u
       | BlockMatchSamplerQCOM -> 65536u
       | ExplicitInterpAMD -> 65536u
       | NodeSharesPayloadLimitsWithAMDX _ -> 65536u
       | NodeMaxPayloadsAMDX _ -> 65536u
       | TrackFinishWritingAMDX -> 65536u
       | PayloadNodeNameAMDX _ -> 65536u
       | PayloadNodeBaseIndexAMDX _ -> 65536u
       | PayloadNodeSparseArrayAMDX -> 65536u
       | PayloadNodeArraySizeAMDX _ -> 65536u
       | PayloadDispatchIndirectAMDX -> 65536u
       | OverrideCoverageNV -> 65536u
       | PassthroughNV -> 65536u
       | ViewportRelativeNV -> 65536u
       | SecondaryViewportRelativeNV _ -> 65536u
       | PerPrimitiveEXT -> 65536u
       | PerViewNV -> 65536u
       | PerTaskNV -> 65536u
       | PerVertexKHR -> 65536u
       | NonUniform -> 66816u
       | RestrictPointer -> 66816u
       | AliasedPointer -> 66816u
       | HitObjectShaderRecordBufferNV -> 65536u
       | BindlessSamplerNV -> 65536u
       | BindlessImageNV -> 65536u
       | BoundSamplerNV -> 65536u
       | BoundImageNV -> 65536u
       | SIMTCallINTEL _ -> 65536u
       | ReferencedIndirectlyINTEL -> 65536u
       | ClobberINTEL _ -> 65536u
       | SideEffectsINTEL -> 65536u
       | VectorComputeVariableINTEL -> 65536u
       | FuncParamIOKindINTEL _ -> 65536u
       | VectorComputeFunctionINTEL -> 65536u
       | StackCallINTEL -> 65536u
       | GlobalVariableOffsetINTEL _ -> 65536u
       | CounterBuffer _ -> 66560u
       | UserSemantic _ -> 66560u
       | UserTypeGOOGLE _ -> 65536u
       | FunctionRoundingModeINTEL _ -> 65536u
       | FunctionDenormModeINTEL _ -> 65536u
       | RegisterINTEL -> 65536u
       | MemoryINTEL _ -> 65536u
       | NumbanksINTEL _ -> 65536u
       | BankwidthINTEL _ -> 65536u
       | MaxPrivateCopiesINTEL _ -> 65536u
       | SinglepumpINTEL -> 65536u
       | DoublepumpINTEL -> 65536u
       | MaxReplicatesINTEL _ -> 65536u
       | SimpleDualPortINTEL -> 65536u
       | MergeINTEL _ -> 65536u
       | BankBitsINTEL _ -> 65536u
       | ForcePow2DepthINTEL _ -> 65536u
       | StridesizeINTEL _ -> 65536u
       | WordsizeINTEL _ -> 65536u
       | TrueDualPortINTEL -> 65536u
       | BurstCoalesceINTEL -> 65536u
       | CacheSizeINTEL _ -> 65536u
       | DontStaticallyCoalesceINTEL -> 65536u
       | PrefetchINTEL _ -> 65536u
       | StallEnableINTEL -> 65536u
       | FuseLoopsInFunctionINTEL -> 65536u
       | MathOpDSPModeINTEL _ -> 65536u
       | AliasScopeINTEL _ -> 65536u
       | NoAliasINTEL _ -> 65536u
       | InitiationIntervalINTEL _ -> 65536u
       | MaxConcurrencyINTEL _ -> 65536u
       | PipelineEnableINTEL _ -> 65536u
       | BufferLocationINTEL _ -> 65536u
       | IOPipeStorageINTEL _ -> 65536u
       | FunctionFloatingPointModeINTEL _ -> 65536u
       | SingleElementVectorINTEL -> 65536u
       | VectorComputeCallableFunctionINTEL -> 65536u
       | MediaBlockIOINTEL -> 65536u
       | StallFreeINTEL -> 65536u
       | FPMaxErrorDecorationINTEL _ -> 65536u
       | LatencyControlLabelINTEL _ -> 65536u
       | LatencyControlConstraintINTEL _ -> 65536u
       | ConduitKernelArgumentINTEL -> 65536u
       | RegisterMapKernelArgumentINTEL -> 65536u
       | MMHostInterfaceAddressWidthINTEL _ -> 65536u
       | MMHostInterfaceDataWidthINTEL _ -> 65536u
       | MMHostInterfaceLatencyINTEL _ -> 65536u
       | MMHostInterfaceReadWriteModeINTEL _ -> 65536u
       | MMHostInterfaceMaxBurstINTEL _ -> 65536u
       | MMHostInterfaceWaitRequestINTEL _ -> 65536u
       | StableKernelArgumentINTEL -> 65536u
       | HostAccessINTEL _ -> 65536u
       | InitModeINTEL _ -> 65536u
       | ImplementInRegisterMapINTEL _ -> 65536u
       | CacheControlLoadINTEL _ -> 65536u
       | CacheControlStoreINTEL _ -> 65536u


type BuiltIn =
   | Position = 0u
   | PointSize = 1u
   | ClipDistance = 3u
   | CullDistance = 4u
   | VertexId = 5u
   | InstanceId = 6u
   | PrimitiveId = 7u
   | InvocationId = 8u
   | Layer = 9u
   | ViewportIndex = 10u
   | TessLevelOuter = 11u
   | TessLevelInner = 12u
   | TessCoord = 13u
   | PatchVertices = 14u
   | FragCoord = 15u
   | PointCoord = 16u
   | FrontFacing = 17u
   | SampleId = 18u
   | SamplePosition = 19u
   | SampleMask = 20u
   | FragDepth = 22u
   | HelperInvocation = 23u
   | NumWorkgroups = 24u
   | WorkgroupSize = 25u
   | WorkgroupId = 26u
   | LocalInvocationId = 27u
   | GlobalInvocationId = 28u
   | LocalInvocationIndex = 29u
   | WorkDim = 30u
   | GlobalSize = 31u
   | EnqueuedWorkgroupSize = 32u
   | GlobalOffset = 33u
   | GlobalLinearId = 34u
   | SubgroupSize = 36u
   | SubgroupMaxSize = 37u
   | NumSubgroups = 38u
   | NumEnqueuedSubgroups = 39u
   | SubgroupId = 40u
   | SubgroupLocalInvocationId = 41u
   | VertexIndex = 42u
   | InstanceIndex = 43u
   | CoreIDARM = 4160u
   | CoreCountARM = 4161u
   | CoreMaxIDARM = 4162u
   | WarpIDARM = 4163u
   | WarpMaxIDARM = 4164u
   | SubgroupEqMask = 4416u
   | SubgroupGeMask = 4417u
   | SubgroupGtMask = 4418u
   | SubgroupLeMask = 4419u
   | SubgroupLtMask = 4420u
   | BaseVertex = 4424u
   | BaseInstance = 4425u
   | DrawIndex = 4426u
   | PrimitiveShadingRateKHR = 4432u
   | DeviceIndex = 4438u
   | ViewIndex = 4440u
   | ShadingRateKHR = 4444u
   | BaryCoordNoPerspAMD = 4992u
   | BaryCoordNoPerspCentroidAMD = 4993u
   | BaryCoordNoPerspSampleAMD = 4994u
   | BaryCoordSmoothAMD = 4995u
   | BaryCoordSmoothCentroidAMD = 4996u
   | BaryCoordSmoothSampleAMD = 4997u
   | BaryCoordPullModelAMD = 4998u
   | FragStencilRefEXT = 5014u
   | RemainingRecursionLevelsAMDX = 5021u
   | ShaderIndexAMDX = 5073u
   | ViewportMaskNV = 5253u
   | SecondaryPositionNV = 5257u
   | SecondaryViewportMaskNV = 5258u
   | PositionPerViewNV = 5261u
   | ViewportMaskPerViewNV = 5262u
   | FullyCoveredEXT = 5264u
   | TaskCountNV = 5274u
   | PrimitiveCountNV = 5275u
   | PrimitiveIndicesNV = 5276u
   | ClipDistancePerViewNV = 5277u
   | CullDistancePerViewNV = 5278u
   | LayerPerViewNV = 5279u
   | MeshViewCountNV = 5280u
   | MeshViewIndicesNV = 5281u
   | BaryCoordKHR = 5286u
   | BaryCoordNoPerspKHR = 5287u
   | FragSizeEXT = 5292u
   | FragInvocationCountEXT = 5293u
   | PrimitivePointIndicesEXT = 5294u
   | PrimitiveLineIndicesEXT = 5295u
   | PrimitiveTriangleIndicesEXT = 5296u
   | CullPrimitiveEXT = 5299u
   | LaunchIdKHR = 5319u
   | LaunchSizeKHR = 5320u
   | WorldRayOriginKHR = 5321u
   | WorldRayDirectionKHR = 5322u
   | ObjectRayOriginKHR = 5323u
   | ObjectRayDirectionKHR = 5324u
   | RayTminKHR = 5325u
   | RayTmaxKHR = 5326u
   | InstanceCustomIndexKHR = 5327u
   | ObjectToWorldKHR = 5330u
   | WorldToObjectKHR = 5331u
   | HitTNV = 5332u
   | HitKindKHR = 5333u
   | CurrentRayTimeNV = 5334u
   | HitTriangleVertexPositionsKHR = 5335u
   | HitMicroTriangleVertexPositionsNV = 5337u
   | HitMicroTriangleVertexBarycentricsNV = 5344u
   | IncomingRayFlagsKHR = 5351u
   | RayGeometryIndexKHR = 5352u
   | WarpsPerSMNV = 5374u
   | SMCountNV = 5375u
   | WarpIDNV = 5376u
   | SMIDNV = 5377u
   | HitKindFrontFacingMicroTriangleNV = 5405u
   | HitKindBackFacingMicroTriangleNV = 5406u
   | CullMaskKHR = 6021u

module BuiltIn =

    let GetVersion x =
       match x with
       | BuiltIn.Position -> 65536u
       | BuiltIn.PointSize -> 65536u
       | BuiltIn.ClipDistance -> 65536u
       | BuiltIn.CullDistance -> 65536u
       | BuiltIn.VertexId -> 65536u
       | BuiltIn.InstanceId -> 65536u
       | BuiltIn.PrimitiveId -> 65536u
       | BuiltIn.InvocationId -> 65536u
       | BuiltIn.Layer -> 65536u
       | BuiltIn.ViewportIndex -> 65536u
       | BuiltIn.TessLevelOuter -> 65536u
       | BuiltIn.TessLevelInner -> 65536u
       | BuiltIn.TessCoord -> 65536u
       | BuiltIn.PatchVertices -> 65536u
       | BuiltIn.FragCoord -> 65536u
       | BuiltIn.PointCoord -> 65536u
       | BuiltIn.FrontFacing -> 65536u
       | BuiltIn.SampleId -> 65536u
       | BuiltIn.SamplePosition -> 65536u
       | BuiltIn.SampleMask -> 65536u
       | BuiltIn.FragDepth -> 65536u
       | BuiltIn.HelperInvocation -> 65536u
       | BuiltIn.NumWorkgroups -> 65536u
       | BuiltIn.WorkgroupSize -> 65536u
       | BuiltIn.WorkgroupId -> 65536u
       | BuiltIn.LocalInvocationId -> 65536u
       | BuiltIn.GlobalInvocationId -> 65536u
       | BuiltIn.LocalInvocationIndex -> 65536u
       | BuiltIn.WorkDim -> 65536u
       | BuiltIn.GlobalSize -> 65536u
       | BuiltIn.EnqueuedWorkgroupSize -> 65536u
       | BuiltIn.GlobalOffset -> 65536u
       | BuiltIn.GlobalLinearId -> 65536u
       | BuiltIn.SubgroupSize -> 65536u
       | BuiltIn.SubgroupMaxSize -> 65536u
       | BuiltIn.NumSubgroups -> 65536u
       | BuiltIn.NumEnqueuedSubgroups -> 65536u
       | BuiltIn.SubgroupId -> 65536u
       | BuiltIn.SubgroupLocalInvocationId -> 65536u
       | BuiltIn.VertexIndex -> 65536u
       | BuiltIn.InstanceIndex -> 65536u
       | BuiltIn.CoreIDARM -> 65536u
       | BuiltIn.CoreCountARM -> 65536u
       | BuiltIn.CoreMaxIDARM -> 65536u
       | BuiltIn.WarpIDARM -> 65536u
       | BuiltIn.WarpMaxIDARM -> 65536u
       | BuiltIn.SubgroupEqMask -> 66304u
       | BuiltIn.SubgroupGeMask -> 66304u
       | BuiltIn.SubgroupGtMask -> 66304u
       | BuiltIn.SubgroupLeMask -> 66304u
       | BuiltIn.SubgroupLtMask -> 66304u
       | BuiltIn.BaseVertex -> 66304u
       | BuiltIn.BaseInstance -> 66304u
       | BuiltIn.DrawIndex -> 66304u
       | BuiltIn.PrimitiveShadingRateKHR -> 65536u
       | BuiltIn.DeviceIndex -> 66304u
       | BuiltIn.ViewIndex -> 66304u
       | BuiltIn.ShadingRateKHR -> 65536u
       | BuiltIn.BaryCoordNoPerspAMD -> 65536u
       | BuiltIn.BaryCoordNoPerspCentroidAMD -> 65536u
       | BuiltIn.BaryCoordNoPerspSampleAMD -> 65536u
       | BuiltIn.BaryCoordSmoothAMD -> 65536u
       | BuiltIn.BaryCoordSmoothCentroidAMD -> 65536u
       | BuiltIn.BaryCoordSmoothSampleAMD -> 65536u
       | BuiltIn.BaryCoordPullModelAMD -> 65536u
       | BuiltIn.FragStencilRefEXT -> 65536u
       | BuiltIn.RemainingRecursionLevelsAMDX -> 65536u
       | BuiltIn.ShaderIndexAMDX -> 65536u
       | BuiltIn.ViewportMaskNV -> 65536u
       | BuiltIn.SecondaryPositionNV -> 65536u
       | BuiltIn.SecondaryViewportMaskNV -> 65536u
       | BuiltIn.PositionPerViewNV -> 65536u
       | BuiltIn.ViewportMaskPerViewNV -> 65536u
       | BuiltIn.FullyCoveredEXT -> 65536u
       | BuiltIn.TaskCountNV -> 65536u
       | BuiltIn.PrimitiveCountNV -> 65536u
       | BuiltIn.PrimitiveIndicesNV -> 65536u
       | BuiltIn.ClipDistancePerViewNV -> 65536u
       | BuiltIn.CullDistancePerViewNV -> 65536u
       | BuiltIn.LayerPerViewNV -> 65536u
       | BuiltIn.MeshViewCountNV -> 65536u
       | BuiltIn.MeshViewIndicesNV -> 65536u
       | BuiltIn.BaryCoordKHR -> 65536u
       | BuiltIn.BaryCoordNoPerspKHR -> 65536u
       | BuiltIn.FragSizeEXT -> 65536u
       | BuiltIn.FragInvocationCountEXT -> 65536u
       | BuiltIn.PrimitivePointIndicesEXT -> 65536u
       | BuiltIn.PrimitiveLineIndicesEXT -> 65536u
       | BuiltIn.PrimitiveTriangleIndicesEXT -> 65536u
       | BuiltIn.CullPrimitiveEXT -> 65536u
       | BuiltIn.LaunchIdKHR -> 65536u
       | BuiltIn.LaunchSizeKHR -> 65536u
       | BuiltIn.WorldRayOriginKHR -> 65536u
       | BuiltIn.WorldRayDirectionKHR -> 65536u
       | BuiltIn.ObjectRayOriginKHR -> 65536u
       | BuiltIn.ObjectRayDirectionKHR -> 65536u
       | BuiltIn.RayTminKHR -> 65536u
       | BuiltIn.RayTmaxKHR -> 65536u
       | BuiltIn.InstanceCustomIndexKHR -> 65536u
       | BuiltIn.ObjectToWorldKHR -> 65536u
       | BuiltIn.WorldToObjectKHR -> 65536u
       | BuiltIn.HitTNV -> 65536u
       | BuiltIn.HitKindKHR -> 65536u
       | BuiltIn.CurrentRayTimeNV -> 65536u
       | BuiltIn.HitTriangleVertexPositionsKHR -> 65536u
       | BuiltIn.HitMicroTriangleVertexPositionsNV -> 65536u
       | BuiltIn.HitMicroTriangleVertexBarycentricsNV -> 65536u
       | BuiltIn.IncomingRayFlagsKHR -> 65536u
       | BuiltIn.RayGeometryIndexKHR -> 65536u
       | BuiltIn.WarpsPerSMNV -> 65536u
       | BuiltIn.SMCountNV -> 65536u
       | BuiltIn.WarpIDNV -> 65536u
       | BuiltIn.SMIDNV -> 65536u
       | BuiltIn.HitKindFrontFacingMicroTriangleNV -> 65536u
       | BuiltIn.HitKindBackFacingMicroTriangleNV -> 65536u
       | BuiltIn.CullMaskKHR -> 65536u

type Scope =
   | CrossDevice = 0u
   | Device = 1u
   | Workgroup = 2u
   | Subgroup = 3u
   | Invocation = 4u
   | QueueFamily = 5u
   | ShaderCallKHR = 6u

module Scope =

    let GetVersion x =
       match x with
       | Scope.CrossDevice -> 65536u
       | Scope.Device -> 65536u
       | Scope.Workgroup -> 65536u
       | Scope.Subgroup -> 65536u
       | Scope.Invocation -> 65536u
       | Scope.QueueFamily -> 66816u
       | Scope.ShaderCallKHR -> 65536u

type GroupOperation =
   | Reduce = 0u
   | InclusiveScan = 1u
   | ExclusiveScan = 2u
   | ClusteredReduce = 3u
   | PartitionedReduceNV = 6u
   | PartitionedInclusiveScanNV = 7u
   | PartitionedExclusiveScanNV = 8u

module GroupOperation =

    let GetVersion x =
       match x with
       | GroupOperation.Reduce -> 65536u
       | GroupOperation.InclusiveScan -> 65536u
       | GroupOperation.ExclusiveScan -> 65536u
       | GroupOperation.ClusteredReduce -> 66304u
       | GroupOperation.PartitionedReduceNV -> 65536u
       | GroupOperation.PartitionedInclusiveScanNV -> 65536u
       | GroupOperation.PartitionedExclusiveScanNV -> 65536u

type KernelEnqueueFlags =
   | NoWait = 0u
   | WaitKernel = 1u
   | WaitWorkGroup = 2u

module KernelEnqueueFlags =

    let GetVersion x =
       match x with
       | KernelEnqueueFlags.NoWait -> 65536u
       | KernelEnqueueFlags.WaitKernel -> 65536u
       | KernelEnqueueFlags.WaitWorkGroup -> 65536u

type Capability =
   | Matrix = 0u
   | Shader = 1u
   | Geometry = 2u
   | Tessellation = 3u
   | Addresses = 4u
   | Linkage = 5u
   | Kernel = 6u
   | Vector16 = 7u
   | Float16Buffer = 8u
   | Float16 = 9u
   | Float64 = 10u
   | Int64 = 11u
   | Int64Atomics = 12u
   | ImageBasic = 13u
   | ImageReadWrite = 14u
   | ImageMipmap = 15u
   | Pipes = 17u
   | Groups = 18u
   | DeviceEnqueue = 19u
   | LiteralSampler = 20u
   | AtomicStorage = 21u
   | Int16 = 22u
   | TessellationPointSize = 23u
   | GeometryPointSize = 24u
   | ImageGatherExtended = 25u
   | StorageImageMultisample = 27u
   | UniformBufferArrayDynamicIndexing = 28u
   | SampledImageArrayDynamicIndexing = 29u
   | StorageBufferArrayDynamicIndexing = 30u
   | StorageImageArrayDynamicIndexing = 31u
   | ClipDistance = 32u
   | CullDistance = 33u
   | ImageCubeArray = 34u
   | SampleRateShading = 35u
   | ImageRect = 36u
   | SampledRect = 37u
   | GenericPointer = 38u
   | Int8 = 39u
   | InputAttachment = 40u
   | SparseResidency = 41u
   | MinLod = 42u
   | Sampled1D = 43u
   | Image1D = 44u
   | SampledCubeArray = 45u
   | SampledBuffer = 46u
   | ImageBuffer = 47u
   | ImageMSArray = 48u
   | StorageImageExtendedFormats = 49u
   | ImageQuery = 50u
   | DerivativeControl = 51u
   | InterpolationFunction = 52u
   | TransformFeedback = 53u
   | GeometryStreams = 54u
   | StorageImageReadWithoutFormat = 55u
   | StorageImageWriteWithoutFormat = 56u
   | MultiViewport = 57u
   | SubgroupDispatch = 58u
   | NamedBarrier = 59u
   | PipeStorage = 60u
   | GroupNonUniform = 61u
   | GroupNonUniformVote = 62u
   | GroupNonUniformArithmetic = 63u
   | GroupNonUniformBallot = 64u
   | GroupNonUniformShuffle = 65u
   | GroupNonUniformShuffleRelative = 66u
   | GroupNonUniformClustered = 67u
   | GroupNonUniformQuad = 68u
   | ShaderLayer = 69u
   | ShaderViewportIndex = 70u
   | UniformDecoration = 71u
   | CoreBuiltinsARM = 4165u
   | TileImageColorReadAccessEXT = 4166u
   | TileImageDepthReadAccessEXT = 4167u
   | TileImageStencilReadAccessEXT = 4168u
   | CooperativeMatrixLayoutsARM = 4201u
   | FragmentShadingRateKHR = 4422u
   | SubgroupBallotKHR = 4423u
   | DrawParameters = 4427u
   | WorkgroupMemoryExplicitLayoutKHR = 4428u
   | WorkgroupMemoryExplicitLayout8BitAccessKHR = 4429u
   | WorkgroupMemoryExplicitLayout16BitAccessKHR = 4430u
   | SubgroupVoteKHR = 4431u
   | StorageBuffer16BitAccess = 4433u
   | UniformAndStorageBuffer16BitAccess = 4434u
   | StoragePushConstant16 = 4435u
   | StorageInputOutput16 = 4436u
   | DeviceGroup = 4437u
   | MultiView = 4439u
   | VariablePointersStorageBuffer = 4441u
   | VariablePointers = 4442u
   | AtomicStorageOps = 4445u
   | SampleMaskPostDepthCoverage = 4447u
   | StorageBuffer8BitAccess = 4448u
   | UniformAndStorageBuffer8BitAccess = 4449u
   | StoragePushConstant8 = 4450u
   | DenormPreserve = 4464u
   | DenormFlushToZero = 4465u
   | SignedZeroInfNanPreserve = 4466u
   | RoundingModeRTE = 4467u
   | RoundingModeRTZ = 4468u
   | RayQueryProvisionalKHR = 4471u
   | RayQueryKHR = 4472u
   | UntypedPointersKHR = 4473u
   | RayTraversalPrimitiveCullingKHR = 4478u
   | RayTracingKHR = 4479u
   | TextureSampleWeightedQCOM = 4484u
   | TextureBoxFilterQCOM = 4485u
   | TextureBlockMatchQCOM = 4486u
   | TextureBlockMatch2QCOM = 4498u
   | Float16ImageAMD = 5008u
   | ImageGatherBiasLodAMD = 5009u
   | FragmentMaskAMD = 5010u
   | StencilExportEXT = 5013u
   | ImageReadWriteLodAMD = 5015u
   | Int64ImageEXT = 5016u
   | ShaderClockKHR = 5055u
   | ShaderEnqueueAMDX = 5067u
   | QuadControlKHR = 5087u
   | SampleMaskOverrideCoverageNV = 5249u
   | GeometryShaderPassthroughNV = 5251u
   | ShaderViewportIndexLayerEXT = 5254u
   | ShaderViewportMaskNV = 5255u
   | ShaderStereoViewNV = 5259u
   | PerViewAttributesNV = 5260u
   | FragmentFullyCoveredEXT = 5265u
   | MeshShadingNV = 5266u
   | ImageFootprintNV = 5282u
   | MeshShadingEXT = 5283u
   | FragmentBarycentricKHR = 5284u
   | ComputeDerivativeGroupQuadsKHR = 5288u
   | FragmentDensityEXT = 5291u
   | GroupNonUniformPartitionedNV = 5297u
   | ShaderNonUniform = 5301u
   | RuntimeDescriptorArray = 5302u
   | InputAttachmentArrayDynamicIndexing = 5303u
   | UniformTexelBufferArrayDynamicIndexing = 5304u
   | StorageTexelBufferArrayDynamicIndexing = 5305u
   | UniformBufferArrayNonUniformIndexing = 5306u
   | SampledImageArrayNonUniformIndexing = 5307u
   | StorageBufferArrayNonUniformIndexing = 5308u
   | StorageImageArrayNonUniformIndexing = 5309u
   | InputAttachmentArrayNonUniformIndexing = 5310u
   | UniformTexelBufferArrayNonUniformIndexing = 5311u
   | StorageTexelBufferArrayNonUniformIndexing = 5312u
   | RayTracingPositionFetchKHR = 5336u
   | RayTracingNV = 5340u
   | RayTracingMotionBlurNV = 5341u
   | VulkanMemoryModel = 5345u
   | VulkanMemoryModelDeviceScope = 5346u
   | PhysicalStorageBufferAddresses = 5347u
   | ComputeDerivativeGroupLinearKHR = 5350u
   | RayTracingProvisionalKHR = 5353u
   | CooperativeMatrixNV = 5357u
   | FragmentShaderSampleInterlockEXT = 5363u
   | FragmentShaderShadingRateInterlockEXT = 5372u
   | ShaderSMBuiltinsNV = 5373u
   | FragmentShaderPixelInterlockEXT = 5378u
   | DemoteToHelperInvocation = 5379u
   | DisplacementMicromapNV = 5380u
   | RayTracingOpacityMicromapEXT = 5381u
   | ShaderInvocationReorderNV = 5383u
   | BindlessTextureNV = 5390u
   | RayQueryPositionFetchKHR = 5391u
   | AtomicFloat16VectorNV = 5404u
   | RayTracingDisplacementMicromapNV = 5409u
   | RawAccessChainsNV = 5414u
   | CooperativeMatrixReductionsNV = 5430u
   | CooperativeMatrixConversionsNV = 5431u
   | CooperativeMatrixPerElementOperationsNV = 5432u
   | CooperativeMatrixTensorAddressingNV = 5433u
   | CooperativeMatrixBlockLoadsNV = 5434u
   | TensorAddressingNV = 5439u
   | SubgroupShuffleINTEL = 5568u
   | SubgroupBufferBlockIOINTEL = 5569u
   | SubgroupImageBlockIOINTEL = 5570u
   | SubgroupImageMediaBlockIOINTEL = 5579u
   | RoundToInfinityINTEL = 5582u
   | FloatingPointModeINTEL = 5583u
   | IntegerFunctions2INTEL = 5584u
   | FunctionPointersINTEL = 5603u
   | IndirectReferencesINTEL = 5604u
   | AsmINTEL = 5606u
   | AtomicFloat32MinMaxEXT = 5612u
   | AtomicFloat64MinMaxEXT = 5613u
   | AtomicFloat16MinMaxEXT = 5616u
   | VectorComputeINTEL = 5617u
   | VectorAnyINTEL = 5619u
   | ExpectAssumeKHR = 5629u
   | SubgroupAvcMotionEstimationINTEL = 5696u
   | SubgroupAvcMotionEstimationIntraINTEL = 5697u
   | SubgroupAvcMotionEstimationChromaINTEL = 5698u
   | VariableLengthArrayINTEL = 5817u
   | FunctionFloatControlINTEL = 5821u
   | FPGAMemoryAttributesINTEL = 5824u
   | FPFastMathModeINTEL = 5837u
   | ArbitraryPrecisionIntegersINTEL = 5844u
   | ArbitraryPrecisionFloatingPointINTEL = 5845u
   | UnstructuredLoopControlsINTEL = 5886u
   | FPGALoopControlsINTEL = 5888u
   | KernelAttributesINTEL = 5892u
   | FPGAKernelAttributesINTEL = 5897u
   | FPGAMemoryAccessesINTEL = 5898u
   | FPGAClusterAttributesINTEL = 5904u
   | LoopFuseINTEL = 5906u
   | FPGADSPControlINTEL = 5908u
   | MemoryAccessAliasingINTEL = 5910u
   | FPGAInvocationPipeliningAttributesINTEL = 5916u
   | FPGABufferLocationINTEL = 5920u
   | ArbitraryPrecisionFixedPointINTEL = 5922u
   | USMStorageClassesINTEL = 5935u
   | RuntimeAlignedAttributeINTEL = 5939u
   | IOPipesINTEL = 5943u
   | BlockingPipesINTEL = 5945u
   | FPGARegINTEL = 5948u
   | DotProductInputAll = 6016u
   | DotProductInput4x8Bit = 6017u
   | DotProductInput4x8BitPacked = 6018u
   | DotProduct = 6019u
   | RayCullMaskKHR = 6020u
   | CooperativeMatrixKHR = 6022u
   | ReplicatedCompositesEXT = 6024u
   | BitInstructions = 6025u
   | GroupNonUniformRotateKHR = 6026u
   | FloatControls2 = 6029u
   | AtomicFloat32AddEXT = 6033u
   | AtomicFloat64AddEXT = 6034u
   | LongCompositesINTEL = 6089u
   | OptNoneEXT = 6094u
   | AtomicFloat16AddEXT = 6095u
   | DebugInfoModuleINTEL = 6114u
   | BFloat16ConversionINTEL = 6115u
   | SplitBarrierINTEL = 6141u
   | ArithmeticFenceEXT = 6144u
   | FPGAClusterAttributesV2INTEL = 6150u
   | FPGAKernelAttributesv2INTEL = 6161u
   | FPMaxErrorINTEL = 6169u
   | FPGALatencyControlINTEL = 6171u
   | FPGAArgumentInterfacesINTEL = 6174u
   | GlobalVariableHostAccessINTEL = 6187u
   | GlobalVariableFPGADecorationsINTEL = 6189u
   | SubgroupBufferPrefetchINTEL = 6220u
   | GroupUniformArithmeticKHR = 6400u
   | MaskedGatherScatterINTEL = 6427u
   | CacheControlsINTEL = 6441u
   | RegisterLimitsINTEL = 6460u

module Capability =

    let GetVersion x =
       match x with
       | Capability.Matrix -> 65536u
       | Capability.Shader -> 65536u
       | Capability.Geometry -> 65536u
       | Capability.Tessellation -> 65536u
       | Capability.Addresses -> 65536u
       | Capability.Linkage -> 65536u
       | Capability.Kernel -> 65536u
       | Capability.Vector16 -> 65536u
       | Capability.Float16Buffer -> 65536u
       | Capability.Float16 -> 65536u
       | Capability.Float64 -> 65536u
       | Capability.Int64 -> 65536u
       | Capability.Int64Atomics -> 65536u
       | Capability.ImageBasic -> 65536u
       | Capability.ImageReadWrite -> 65536u
       | Capability.ImageMipmap -> 65536u
       | Capability.Pipes -> 65536u
       | Capability.Groups -> 65536u
       | Capability.DeviceEnqueue -> 65536u
       | Capability.LiteralSampler -> 65536u
       | Capability.AtomicStorage -> 65536u
       | Capability.Int16 -> 65536u
       | Capability.TessellationPointSize -> 65536u
       | Capability.GeometryPointSize -> 65536u
       | Capability.ImageGatherExtended -> 65536u
       | Capability.StorageImageMultisample -> 65536u
       | Capability.UniformBufferArrayDynamicIndexing -> 65536u
       | Capability.SampledImageArrayDynamicIndexing -> 65536u
       | Capability.StorageBufferArrayDynamicIndexing -> 65536u
       | Capability.StorageImageArrayDynamicIndexing -> 65536u
       | Capability.ClipDistance -> 65536u
       | Capability.CullDistance -> 65536u
       | Capability.ImageCubeArray -> 65536u
       | Capability.SampleRateShading -> 65536u
       | Capability.ImageRect -> 65536u
       | Capability.SampledRect -> 65536u
       | Capability.GenericPointer -> 65536u
       | Capability.Int8 -> 65536u
       | Capability.InputAttachment -> 65536u
       | Capability.SparseResidency -> 65536u
       | Capability.MinLod -> 65536u
       | Capability.Sampled1D -> 65536u
       | Capability.Image1D -> 65536u
       | Capability.SampledCubeArray -> 65536u
       | Capability.SampledBuffer -> 65536u
       | Capability.ImageBuffer -> 65536u
       | Capability.ImageMSArray -> 65536u
       | Capability.StorageImageExtendedFormats -> 65536u
       | Capability.ImageQuery -> 65536u
       | Capability.DerivativeControl -> 65536u
       | Capability.InterpolationFunction -> 65536u
       | Capability.TransformFeedback -> 65536u
       | Capability.GeometryStreams -> 65536u
       | Capability.StorageImageReadWithoutFormat -> 65536u
       | Capability.StorageImageWriteWithoutFormat -> 65536u
       | Capability.MultiViewport -> 65536u
       | Capability.SubgroupDispatch -> 65792u
       | Capability.NamedBarrier -> 65792u
       | Capability.PipeStorage -> 65792u
       | Capability.GroupNonUniform -> 66304u
       | Capability.GroupNonUniformVote -> 66304u
       | Capability.GroupNonUniformArithmetic -> 66304u
       | Capability.GroupNonUniformBallot -> 66304u
       | Capability.GroupNonUniformShuffle -> 66304u
       | Capability.GroupNonUniformShuffleRelative -> 66304u
       | Capability.GroupNonUniformClustered -> 66304u
       | Capability.GroupNonUniformQuad -> 66304u
       | Capability.ShaderLayer -> 66816u
       | Capability.ShaderViewportIndex -> 66816u
       | Capability.UniformDecoration -> 67072u
       | Capability.CoreBuiltinsARM -> 65536u
       | Capability.TileImageColorReadAccessEXT -> 65536u
       | Capability.TileImageDepthReadAccessEXT -> 65536u
       | Capability.TileImageStencilReadAccessEXT -> 65536u
       | Capability.CooperativeMatrixLayoutsARM -> 65536u
       | Capability.FragmentShadingRateKHR -> 65536u
       | Capability.SubgroupBallotKHR -> 65536u
       | Capability.DrawParameters -> 66304u
       | Capability.WorkgroupMemoryExplicitLayoutKHR -> 65536u
       | Capability.WorkgroupMemoryExplicitLayout8BitAccessKHR -> 65536u
       | Capability.WorkgroupMemoryExplicitLayout16BitAccessKHR -> 65536u
       | Capability.SubgroupVoteKHR -> 65536u
       | Capability.StorageBuffer16BitAccess -> 66304u
       | Capability.UniformAndStorageBuffer16BitAccess -> 66304u
       | Capability.StoragePushConstant16 -> 66304u
       | Capability.StorageInputOutput16 -> 66304u
       | Capability.DeviceGroup -> 66304u
       | Capability.MultiView -> 66304u
       | Capability.VariablePointersStorageBuffer -> 66304u
       | Capability.VariablePointers -> 66304u
       | Capability.AtomicStorageOps -> 65536u
       | Capability.SampleMaskPostDepthCoverage -> 65536u
       | Capability.StorageBuffer8BitAccess -> 66816u
       | Capability.UniformAndStorageBuffer8BitAccess -> 66816u
       | Capability.StoragePushConstant8 -> 66816u
       | Capability.DenormPreserve -> 66560u
       | Capability.DenormFlushToZero -> 66560u
       | Capability.SignedZeroInfNanPreserve -> 66560u
       | Capability.RoundingModeRTE -> 66560u
       | Capability.RoundingModeRTZ -> 66560u
       | Capability.RayQueryProvisionalKHR -> 65536u
       | Capability.RayQueryKHR -> 65536u
       | Capability.UntypedPointersKHR -> 65536u
       | Capability.RayTraversalPrimitiveCullingKHR -> 65536u
       | Capability.RayTracingKHR -> 65536u
       | Capability.TextureSampleWeightedQCOM -> 65536u
       | Capability.TextureBoxFilterQCOM -> 65536u
       | Capability.TextureBlockMatchQCOM -> 65536u
       | Capability.TextureBlockMatch2QCOM -> 65536u
       | Capability.Float16ImageAMD -> 65536u
       | Capability.ImageGatherBiasLodAMD -> 65536u
       | Capability.FragmentMaskAMD -> 65536u
       | Capability.StencilExportEXT -> 65536u
       | Capability.ImageReadWriteLodAMD -> 65536u
       | Capability.Int64ImageEXT -> 65536u
       | Capability.ShaderClockKHR -> 65536u
       | Capability.ShaderEnqueueAMDX -> 65536u
       | Capability.QuadControlKHR -> 65536u
       | Capability.SampleMaskOverrideCoverageNV -> 65536u
       | Capability.GeometryShaderPassthroughNV -> 65536u
       | Capability.ShaderViewportIndexLayerEXT -> 65536u
       | Capability.ShaderViewportMaskNV -> 65536u
       | Capability.ShaderStereoViewNV -> 65536u
       | Capability.PerViewAttributesNV -> 65536u
       | Capability.FragmentFullyCoveredEXT -> 65536u
       | Capability.MeshShadingNV -> 65536u
       | Capability.ImageFootprintNV -> 65536u
       | Capability.MeshShadingEXT -> 65536u
       | Capability.FragmentBarycentricKHR -> 65536u
       | Capability.ComputeDerivativeGroupQuadsKHR -> 65536u
       | Capability.FragmentDensityEXT -> 65536u
       | Capability.GroupNonUniformPartitionedNV -> 65536u
       | Capability.ShaderNonUniform -> 66816u
       | Capability.RuntimeDescriptorArray -> 66816u
       | Capability.InputAttachmentArrayDynamicIndexing -> 66816u
       | Capability.UniformTexelBufferArrayDynamicIndexing -> 66816u
       | Capability.StorageTexelBufferArrayDynamicIndexing -> 66816u
       | Capability.UniformBufferArrayNonUniformIndexing -> 66816u
       | Capability.SampledImageArrayNonUniformIndexing -> 66816u
       | Capability.StorageBufferArrayNonUniformIndexing -> 66816u
       | Capability.StorageImageArrayNonUniformIndexing -> 66816u
       | Capability.InputAttachmentArrayNonUniformIndexing -> 66816u
       | Capability.UniformTexelBufferArrayNonUniformIndexing -> 66816u
       | Capability.StorageTexelBufferArrayNonUniformIndexing -> 66816u
       | Capability.RayTracingPositionFetchKHR -> 65536u
       | Capability.RayTracingNV -> 65536u
       | Capability.RayTracingMotionBlurNV -> 65536u
       | Capability.VulkanMemoryModel -> 66816u
       | Capability.VulkanMemoryModelDeviceScope -> 66816u
       | Capability.PhysicalStorageBufferAddresses -> 66816u
       | Capability.ComputeDerivativeGroupLinearKHR -> 65536u
       | Capability.RayTracingProvisionalKHR -> 65536u
       | Capability.CooperativeMatrixNV -> 65536u
       | Capability.FragmentShaderSampleInterlockEXT -> 65536u
       | Capability.FragmentShaderShadingRateInterlockEXT -> 65536u
       | Capability.ShaderSMBuiltinsNV -> 65536u
       | Capability.FragmentShaderPixelInterlockEXT -> 65536u
       | Capability.DemoteToHelperInvocation -> 67072u
       | Capability.DisplacementMicromapNV -> 65536u
       | Capability.RayTracingOpacityMicromapEXT -> 65536u
       | Capability.ShaderInvocationReorderNV -> 65536u
       | Capability.BindlessTextureNV -> 65536u
       | Capability.RayQueryPositionFetchKHR -> 65536u
       | Capability.AtomicFloat16VectorNV -> 65536u
       | Capability.RayTracingDisplacementMicromapNV -> 65536u
       | Capability.RawAccessChainsNV -> 65536u
       | Capability.CooperativeMatrixReductionsNV -> 65536u
       | Capability.CooperativeMatrixConversionsNV -> 65536u
       | Capability.CooperativeMatrixPerElementOperationsNV -> 65536u
       | Capability.CooperativeMatrixTensorAddressingNV -> 65536u
       | Capability.CooperativeMatrixBlockLoadsNV -> 65536u
       | Capability.TensorAddressingNV -> 65536u
       | Capability.SubgroupShuffleINTEL -> 65536u
       | Capability.SubgroupBufferBlockIOINTEL -> 65536u
       | Capability.SubgroupImageBlockIOINTEL -> 65536u
       | Capability.SubgroupImageMediaBlockIOINTEL -> 65536u
       | Capability.RoundToInfinityINTEL -> 65536u
       | Capability.FloatingPointModeINTEL -> 65536u
       | Capability.IntegerFunctions2INTEL -> 65536u
       | Capability.FunctionPointersINTEL -> 65536u
       | Capability.IndirectReferencesINTEL -> 65536u
       | Capability.AsmINTEL -> 65536u
       | Capability.AtomicFloat32MinMaxEXT -> 65536u
       | Capability.AtomicFloat64MinMaxEXT -> 65536u
       | Capability.AtomicFloat16MinMaxEXT -> 65536u
       | Capability.VectorComputeINTEL -> 65536u
       | Capability.VectorAnyINTEL -> 65536u
       | Capability.ExpectAssumeKHR -> 65536u
       | Capability.SubgroupAvcMotionEstimationINTEL -> 65536u
       | Capability.SubgroupAvcMotionEstimationIntraINTEL -> 65536u
       | Capability.SubgroupAvcMotionEstimationChromaINTEL -> 65536u
       | Capability.VariableLengthArrayINTEL -> 65536u
       | Capability.FunctionFloatControlINTEL -> 65536u
       | Capability.FPGAMemoryAttributesINTEL -> 65536u
       | Capability.FPFastMathModeINTEL -> 65536u
       | Capability.ArbitraryPrecisionIntegersINTEL -> 65536u
       | Capability.ArbitraryPrecisionFloatingPointINTEL -> 65536u
       | Capability.UnstructuredLoopControlsINTEL -> 65536u
       | Capability.FPGALoopControlsINTEL -> 65536u
       | Capability.KernelAttributesINTEL -> 65536u
       | Capability.FPGAKernelAttributesINTEL -> 65536u
       | Capability.FPGAMemoryAccessesINTEL -> 65536u
       | Capability.FPGAClusterAttributesINTEL -> 65536u
       | Capability.LoopFuseINTEL -> 65536u
       | Capability.FPGADSPControlINTEL -> 65536u
       | Capability.MemoryAccessAliasingINTEL -> 65536u
       | Capability.FPGAInvocationPipeliningAttributesINTEL -> 65536u
       | Capability.FPGABufferLocationINTEL -> 65536u
       | Capability.ArbitraryPrecisionFixedPointINTEL -> 65536u
       | Capability.USMStorageClassesINTEL -> 65536u
       | Capability.RuntimeAlignedAttributeINTEL -> 65536u
       | Capability.IOPipesINTEL -> 65536u
       | Capability.BlockingPipesINTEL -> 65536u
       | Capability.FPGARegINTEL -> 65536u
       | Capability.DotProductInputAll -> 67072u
       | Capability.DotProductInput4x8Bit -> 67072u
       | Capability.DotProductInput4x8BitPacked -> 67072u
       | Capability.DotProduct -> 67072u
       | Capability.RayCullMaskKHR -> 65536u
       | Capability.CooperativeMatrixKHR -> 65536u
       | Capability.ReplicatedCompositesEXT -> 65536u
       | Capability.BitInstructions -> 65536u
       | Capability.GroupNonUniformRotateKHR -> 65536u
       | Capability.FloatControls2 -> 65536u
       | Capability.AtomicFloat32AddEXT -> 65536u
       | Capability.AtomicFloat64AddEXT -> 65536u
       | Capability.LongCompositesINTEL -> 65536u
       | Capability.OptNoneEXT -> 65536u
       | Capability.AtomicFloat16AddEXT -> 65536u
       | Capability.DebugInfoModuleINTEL -> 65536u
       | Capability.BFloat16ConversionINTEL -> 65536u
       | Capability.SplitBarrierINTEL -> 65536u
       | Capability.ArithmeticFenceEXT -> 65536u
       | Capability.FPGAClusterAttributesV2INTEL -> 65536u
       | Capability.FPGAKernelAttributesv2INTEL -> 65536u
       | Capability.FPMaxErrorINTEL -> 65536u
       | Capability.FPGALatencyControlINTEL -> 65536u
       | Capability.FPGAArgumentInterfacesINTEL -> 65536u
       | Capability.GlobalVariableHostAccessINTEL -> 65536u
       | Capability.GlobalVariableFPGADecorationsINTEL -> 65536u
       | Capability.SubgroupBufferPrefetchINTEL -> 65536u
       | Capability.GroupUniformArithmeticKHR -> 65536u
       | Capability.MaskedGatherScatterINTEL -> 65536u
       | Capability.CacheControlsINTEL -> 65536u
       | Capability.RegisterLimitsINTEL -> 65536u

type RayQueryIntersection =
   | RayQueryCandidateIntersectionKHR = 0u
   | RayQueryCommittedIntersectionKHR = 1u

module RayQueryIntersection =

    let GetVersion x =
       match x with
       | RayQueryIntersection.RayQueryCandidateIntersectionKHR -> 65536u
       | RayQueryIntersection.RayQueryCommittedIntersectionKHR -> 65536u

type RayQueryCommittedIntersectionType =
   | RayQueryCommittedIntersectionNoneKHR = 0u
   | RayQueryCommittedIntersectionTriangleKHR = 1u
   | RayQueryCommittedIntersectionGeneratedKHR = 2u

module RayQueryCommittedIntersectionType =

    let GetVersion x =
       match x with
       | RayQueryCommittedIntersectionType.RayQueryCommittedIntersectionNoneKHR -> 65536u
       | RayQueryCommittedIntersectionType.RayQueryCommittedIntersectionTriangleKHR -> 65536u
       | RayQueryCommittedIntersectionType.RayQueryCommittedIntersectionGeneratedKHR -> 65536u

type RayQueryCandidateIntersectionType =
   | RayQueryCandidateIntersectionTriangleKHR = 0u
   | RayQueryCandidateIntersectionAABBKHR = 1u

module RayQueryCandidateIntersectionType =

    let GetVersion x =
       match x with
       | RayQueryCandidateIntersectionType.RayQueryCandidateIntersectionTriangleKHR -> 65536u
       | RayQueryCandidateIntersectionType.RayQueryCandidateIntersectionAABBKHR -> 65536u

type PackedVectorFormat =
   | PackedVectorFormat4x8Bit = 0u

module PackedVectorFormat =

    let GetVersion x =
       match x with
       | PackedVectorFormat.PackedVectorFormat4x8Bit -> 67072u

type CooperativeMatrixOperands =
   | NoneKHR = 0x0000u
   | MatrixASignedComponentsKHR = 0x0001u
   | MatrixBSignedComponentsKHR = 0x0002u
   | MatrixCSignedComponentsKHR = 0x0004u
   | MatrixResultSignedComponentsKHR = 0x0008u
   | SaturatingAccumulationKHR = 0x0010u

module CooperativeMatrixOperands =

    let GetVersion x =
       match x with
       | CooperativeMatrixOperands.NoneKHR -> 65536u
       | CooperativeMatrixOperands.MatrixASignedComponentsKHR -> 65536u
       | CooperativeMatrixOperands.MatrixBSignedComponentsKHR -> 65536u
       | CooperativeMatrixOperands.MatrixCSignedComponentsKHR -> 65536u
       | CooperativeMatrixOperands.MatrixResultSignedComponentsKHR -> 65536u
       | CooperativeMatrixOperands.SaturatingAccumulationKHR -> 65536u

type CooperativeMatrixLayout =
   | RowMajorKHR = 0u
   | ColumnMajorKHR = 1u
   | RowBlockedInterleavedARM = 4202u
   | ColumnBlockedInterleavedARM = 4203u

module CooperativeMatrixLayout =

    let GetVersion x =
       match x with
       | CooperativeMatrixLayout.RowMajorKHR -> 65536u
       | CooperativeMatrixLayout.ColumnMajorKHR -> 65536u
       | CooperativeMatrixLayout.RowBlockedInterleavedARM -> 65536u
       | CooperativeMatrixLayout.ColumnBlockedInterleavedARM -> 65536u

type CooperativeMatrixUse =
   | MatrixAKHR = 0u
   | MatrixBKHR = 1u
   | MatrixAccumulatorKHR = 2u

module CooperativeMatrixUse =

    let GetVersion x =
       match x with
       | CooperativeMatrixUse.MatrixAKHR -> 65536u
       | CooperativeMatrixUse.MatrixBKHR -> 65536u
       | CooperativeMatrixUse.MatrixAccumulatorKHR -> 65536u

type CooperativeMatrixReduce =
   | Row = 0x0001u
   | Column = 0x0002u
   | TwoByTwo = 0x0004u

module CooperativeMatrixReduce =

    let GetVersion x =
       match x with
       | CooperativeMatrixReduce.Row -> 65536u
       | CooperativeMatrixReduce.Column -> 65536u
       | CooperativeMatrixReduce.TwoByTwo -> 65536u

type TensorClampMode =
   | Undefined = 0u
   | Constant = 1u
   | ClampToEdge = 2u
   | Repeat = 3u
   | RepeatMirrored = 4u

module TensorClampMode =

    let GetVersion x =
       match x with
       | TensorClampMode.Undefined -> 65536u
       | TensorClampMode.Constant -> 65536u
       | TensorClampMode.ClampToEdge -> 65536u
       | TensorClampMode.Repeat -> 65536u
       | TensorClampMode.RepeatMirrored -> 65536u

[<RequireQualifiedAccess>]
type TensorAddressingOperands =
    | None
    | TensorView of IdRef
    | DecodeFunc of IdRef

    member x.Value =
       match x with
       | None -> 0x0000u
       | TensorView _ -> 0x0001u
       | DecodeFunc _ -> 0x0002u

    member x.Version =
       match x with
       | None -> 65536u
       | TensorView _ -> 65536u
       | DecodeFunc _ -> 65536u


type InitializationModeQualifier =
   | InitOnDeviceReprogramINTEL = 0u
   | InitOnDeviceResetINTEL = 1u

module InitializationModeQualifier =

    let GetVersion x =
       match x with
       | InitializationModeQualifier.InitOnDeviceReprogramINTEL -> 65536u
       | InitializationModeQualifier.InitOnDeviceResetINTEL -> 65536u

type LoadCacheControl =
   | UncachedINTEL = 0u
   | CachedINTEL = 1u
   | StreamingINTEL = 2u
   | InvalidateAfterReadINTEL = 3u
   | ConstCachedINTEL = 4u

module LoadCacheControl =

    let GetVersion x =
       match x with
       | LoadCacheControl.UncachedINTEL -> 65536u
       | LoadCacheControl.CachedINTEL -> 65536u
       | LoadCacheControl.StreamingINTEL -> 65536u
       | LoadCacheControl.InvalidateAfterReadINTEL -> 65536u
       | LoadCacheControl.ConstCachedINTEL -> 65536u

type StoreCacheControl =
   | UncachedINTEL = 0u
   | WriteThroughINTEL = 1u
   | WriteBackINTEL = 2u
   | StreamingINTEL = 3u

module StoreCacheControl =

    let GetVersion x =
       match x with
       | StoreCacheControl.UncachedINTEL -> 65536u
       | StoreCacheControl.WriteThroughINTEL -> 65536u
       | StoreCacheControl.WriteBackINTEL -> 65536u
       | StoreCacheControl.StreamingINTEL -> 65536u

type NamedMaximumNumberOfRegisters =
   | AutoINTEL = 0u

module NamedMaximumNumberOfRegisters =

    let GetVersion x =
       match x with
       | NamedMaximumNumberOfRegisters.AutoINTEL -> 65536u

type FPEncoding =
   | FPEncoding = 0u
/// Reference to an <id> representing the result's type of the enclosing instruction
type IdResultType = uint32

/// Definition of an <id> representing the result of the enclosing instruction
type IdResult = uint32

/// Reference to an <id> representing a 32-bit integer that is a mask from the MemorySemantics operand kind
type IdMemorySemantics = uint32

/// Reference to an <id> representing a 32-bit integer that is a mask from the Scope operand kind
type IdScope = uint32

/// Reference to an <id>
type IdRef = uint32

/// An integer consuming one or more words
type LiteralInteger = uint32

/// A null-terminated stream of characters consuming an integral number of words
type LiteralString = string

/// A float consuming one word
type LiteralFloat = uint32

/// A literal number whose size and format are determined by a previous operand in the enclosing instruction
type LiteralContextDependentNumber = uint32

/// A 32-bit unsigned integer indicating which instruction to use and determining the layout of following operands (for OpExtInst)
type LiteralExtInstInteger = uint32

/// An opcode indicating the operation to be performed and determining the layout of following operands (for OpSpecConstantOp)
type LiteralSpecConstantOpInteger = uint32

type PairLiteralIntegerIdRef = PairLiteralIntegerIdRef of LiteralInteger * IdRef

type PairIdRefLiteralInteger = PairIdRefLiteralInteger of IdRef * LiteralInteger

type PairIdRefIdRef = PairIdRefIdRef of IdRef * IdRef

type Instruction =
   | OpNop
   | OpUndef of IdResultType * IdResult
   | OpSourceContinued of ContinuedSource: LiteralString
   | OpSource of SourceLanguage * Version: LiteralInteger * File: IdRef option * Source: LiteralString option
   | OpSourceExtension of Extension: LiteralString
   | OpName of Target: IdRef * Name: LiteralString
   | OpMemberName of Type: IdRef * Member: LiteralInteger * Name: LiteralString
   | OpString of IdResult * String: LiteralString
   | OpLine of File: IdRef * Line: LiteralInteger * Column: LiteralInteger
   | OpExtension of Name: LiteralString
   | OpExtInstImport of IdResult * Name: LiteralString
   | OpExtInst of IdResultType * IdResult * Set: IdRef * Instruction: LiteralExtInstInteger * Operand1: IdRef list
   | OpMemoryModel of AddressingModel * MemoryModel
   | OpEntryPoint of ExecutionModel * EntryPoint: IdRef * Name: LiteralString * Interface: IdRef list
   | OpExecutionMode of EntryPoint: IdRef * Mode: ExecutionMode
   | OpCapability of Capability: Capability
   | OpTypeVoid of IdResult
   | OpTypeBool of IdResult
   | OpTypeInt of IdResult * Width: LiteralInteger * Signedness: LiteralInteger
   | OpTypeFloat of IdResult * Width: LiteralInteger * FloatingPointEncoding: FPEncoding option
   | OpTypeVector of IdResult * ComponentType: IdRef * ComponentCount: LiteralInteger
   | OpTypeMatrix of IdResult * ColumnType: IdRef * ColumnCount: LiteralInteger
   | OpTypeImage of IdResult * SampledType: IdRef * Dim * Depth: LiteralInteger * Arrayed: LiteralInteger * MS: LiteralInteger * Sampled: LiteralInteger * ImageFormat * AccessQualifier option
   | OpTypeSampler of IdResult
   | OpTypeSampledImage of IdResult * ImageType: IdRef
   | OpTypeArray of IdResult * ElementType: IdRef * Length: IdRef
   | OpTypeRuntimeArray of IdResult * ElementType: IdRef
   | OpTypeStruct of IdResult * Member0type: IdRef list
   | OpTypeOpaque of IdResult * LiteralString
   | OpTypePointer of IdResult * StorageClass * Type: IdRef
   | OpTypeFunction of IdResult * ReturnType: IdRef * Parameter0Type: IdRef list
   | OpTypeEvent of IdResult
   | OpTypeDeviceEvent of IdResult
   | OpTypeReserveId of IdResult
   | OpTypeQueue of IdResult
   | OpTypePipe of IdResult * Qualifier: AccessQualifier
   | OpTypeForwardPointer of PointerType: IdRef * StorageClass
   | OpConstantTrue of IdResultType * IdResult
   | OpConstantFalse of IdResultType * IdResult
   | OpConstant of IdResultType * IdResult * Value: LiteralContextDependentNumber
   | OpConstantComposite of IdResultType * IdResult * Constituents: IdRef list
   | OpConstantSampler of IdResultType * IdResult * SamplerAddressingMode * Param: LiteralInteger * SamplerFilterMode
   | OpConstantNull of IdResultType * IdResult
   | OpSpecConstantTrue of IdResultType * IdResult
   | OpSpecConstantFalse of IdResultType * IdResult
   | OpSpecConstant of IdResultType * IdResult * Value: LiteralContextDependentNumber
   | OpSpecConstantComposite of IdResultType * IdResult * Constituents: IdRef list
   | OpSpecConstantOp of IdResultType * IdResult * Opcode: LiteralSpecConstantOpInteger
   | OpFunction of IdResultType * IdResult * FunctionControl * FunctionType: IdRef
   | OpFunctionParameter of IdResultType * IdResult
   | OpFunctionEnd
   | OpFunctionCall of IdResultType * IdResult * Function: IdRef * Argument0: IdRef list
   | OpVariable of IdResultType * IdResult * StorageClass * Initializer: IdRef option
   | OpImageTexelPointer of IdResultType * IdResult * Image: IdRef * Coordinate: IdRef * Sample: IdRef
   | OpLoad of IdResultType * IdResult * Pointer: IdRef * MemoryAccess option
   | OpStore of Pointer: IdRef * Object: IdRef * MemoryAccess option
   | OpCopyMemory of Target: IdRef * Source: IdRef * MemoryAccess option * MemoryAccess option
   | OpCopyMemorySized of Target: IdRef * Source: IdRef * Size: IdRef * MemoryAccess option * MemoryAccess option
   | OpAccessChain of IdResultType * IdResult * Base: IdRef * Indexes: IdRef list
   | OpInBoundsAccessChain of IdResultType * IdResult * Base: IdRef * Indexes: IdRef list
   | OpPtrAccessChain of IdResultType * IdResult * Base: IdRef * Element: IdRef * Indexes: IdRef list
   | OpArrayLength of IdResultType * IdResult * Structure: IdRef * Arraymember: LiteralInteger
   | OpGenericPtrMemSemantics of IdResultType * IdResult * Pointer: IdRef
   | OpInBoundsPtrAccessChain of IdResultType * IdResult * Base: IdRef * Element: IdRef * Indexes: IdRef list
   | OpDecorate of Target: IdRef * Decoration
   | OpMemberDecorate of StructureType: IdRef * Member: LiteralInteger * Decoration
   | OpDecorationGroup of IdResult
   | OpGroupDecorate of DecorationGroup: IdRef * Targets: IdRef list
   | OpGroupMemberDecorate of DecorationGroup: IdRef * Targets: PairIdRefLiteralInteger list
   | OpVectorExtractDynamic of IdResultType * IdResult * Vector: IdRef * Index: IdRef
   | OpVectorInsertDynamic of IdResultType * IdResult * Vector: IdRef * Component: IdRef * Index: IdRef
   | OpVectorShuffle of IdResultType * IdResult * Vector1: IdRef * Vector2: IdRef * Components: LiteralInteger list
   | OpCompositeConstruct of IdResultType * IdResult * Constituents: IdRef list
   | OpCompositeExtract of IdResultType * IdResult * Composite: IdRef * Indexes: LiteralInteger list
   | OpCompositeInsert of IdResultType * IdResult * Object: IdRef * Composite: IdRef * Indexes: LiteralInteger list
   | OpCopyObject of IdResultType * IdResult * Operand: IdRef
   | OpTranspose of IdResultType * IdResult * Matrix: IdRef
   | OpSampledImage of IdResultType * IdResult * Image: IdRef * Sampler: IdRef
   | OpImageSampleImplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * ImageOperands option
   | OpImageSampleExplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * ImageOperands
   | OpImageSampleDrefImplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Dref: IdRef * ImageOperands option
   | OpImageSampleDrefExplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Dref: IdRef * ImageOperands
   | OpImageSampleProjImplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * ImageOperands option
   | OpImageSampleProjExplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * ImageOperands
   | OpImageSampleProjDrefImplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Dref: IdRef * ImageOperands option
   | OpImageSampleProjDrefExplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Dref: IdRef * ImageOperands
   | OpImageFetch of IdResultType * IdResult * Image: IdRef * Coordinate: IdRef * ImageOperands option
   | OpImageGather of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Component: IdRef * ImageOperands option
   | OpImageDrefGather of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Dref: IdRef * ImageOperands option
   | OpImageRead of IdResultType * IdResult * Image: IdRef * Coordinate: IdRef * ImageOperands option
   | OpImageWrite of Image: IdRef * Coordinate: IdRef * Texel: IdRef * ImageOperands option
   | OpImage of IdResultType * IdResult * SampledImage: IdRef
   | OpImageQueryFormat of IdResultType * IdResult * Image: IdRef
   | OpImageQueryOrder of IdResultType * IdResult * Image: IdRef
   | OpImageQuerySizeLod of IdResultType * IdResult * Image: IdRef * LevelofDetail: IdRef
   | OpImageQuerySize of IdResultType * IdResult * Image: IdRef
   | OpImageQueryLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef
   | OpImageQueryLevels of IdResultType * IdResult * Image: IdRef
   | OpImageQuerySamples of IdResultType * IdResult * Image: IdRef
   | OpConvertFToU of IdResultType * IdResult * FloatValue: IdRef
   | OpConvertFToS of IdResultType * IdResult * FloatValue: IdRef
   | OpConvertSToF of IdResultType * IdResult * SignedValue: IdRef
   | OpConvertUToF of IdResultType * IdResult * UnsignedValue: IdRef
   | OpUConvert of IdResultType * IdResult * UnsignedValue: IdRef
   | OpSConvert of IdResultType * IdResult * SignedValue: IdRef
   | OpFConvert of IdResultType * IdResult * FloatValue: IdRef
   | OpQuantizeToF16 of IdResultType * IdResult * Value: IdRef
   | OpConvertPtrToU of IdResultType * IdResult * Pointer: IdRef
   | OpSatConvertSToU of IdResultType * IdResult * SignedValue: IdRef
   | OpSatConvertUToS of IdResultType * IdResult * UnsignedValue: IdRef
   | OpConvertUToPtr of IdResultType * IdResult * IntegerValue: IdRef
   | OpPtrCastToGeneric of IdResultType * IdResult * Pointer: IdRef
   | OpGenericCastToPtr of IdResultType * IdResult * Pointer: IdRef
   | OpGenericCastToPtrExplicit of IdResultType * IdResult * Pointer: IdRef * Storage: StorageClass
   | OpBitcast of IdResultType * IdResult * Operand: IdRef
   | OpSNegate of IdResultType * IdResult * Operand: IdRef
   | OpFNegate of IdResultType * IdResult * Operand: IdRef
   | OpIAdd of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFAdd of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpISub of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFSub of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpIMul of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFMul of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpUDiv of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpSDiv of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFDiv of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpUMod of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpSRem of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpSMod of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFRem of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFMod of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpVectorTimesScalar of IdResultType * IdResult * Vector: IdRef * Scalar: IdRef
   | OpMatrixTimesScalar of IdResultType * IdResult * Matrix: IdRef * Scalar: IdRef
   | OpVectorTimesMatrix of IdResultType * IdResult * Vector: IdRef * Matrix: IdRef
   | OpMatrixTimesVector of IdResultType * IdResult * Matrix: IdRef * Vector: IdRef
   | OpMatrixTimesMatrix of IdResultType * IdResult * LeftMatrix: IdRef * RightMatrix: IdRef
   | OpOuterProduct of IdResultType * IdResult * Vector1: IdRef * Vector2: IdRef
   | OpDot of IdResultType * IdResult * Vector1: IdRef * Vector2: IdRef
   | OpIAddCarry of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpISubBorrow of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpUMulExtended of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpSMulExtended of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpAny of IdResultType * IdResult * Vector: IdRef
   | OpAll of IdResultType * IdResult * Vector: IdRef
   | OpIsNan of IdResultType * IdResult * x: IdRef
   | OpIsInf of IdResultType * IdResult * x: IdRef
   | OpIsFinite of IdResultType * IdResult * x: IdRef
   | OpIsNormal of IdResultType * IdResult * x: IdRef
   | OpSignBitSet of IdResultType * IdResult * x: IdRef
   | OpLessOrGreater of IdResultType * IdResult * x: IdRef * y: IdRef
   | OpOrdered of IdResultType * IdResult * x: IdRef * y: IdRef
   | OpUnordered of IdResultType * IdResult * x: IdRef * y: IdRef
   | OpLogicalEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpLogicalNotEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpLogicalOr of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpLogicalAnd of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpLogicalNot of IdResultType * IdResult * Operand: IdRef
   | OpSelect of IdResultType * IdResult * Condition: IdRef * Object1: IdRef * Object2: IdRef
   | OpIEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpINotEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpUGreaterThan of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpSGreaterThan of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpUGreaterThanEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpSGreaterThanEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpULessThan of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpSLessThan of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpULessThanEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpSLessThanEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFOrdEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFUnordEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFOrdNotEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFUnordNotEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFOrdLessThan of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFUnordLessThan of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFOrdGreaterThan of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFUnordGreaterThan of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFOrdLessThanEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFUnordLessThanEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFOrdGreaterThanEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpFUnordGreaterThanEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpShiftRightLogical of IdResultType * IdResult * Base: IdRef * Shift: IdRef
   | OpShiftRightArithmetic of IdResultType * IdResult * Base: IdRef * Shift: IdRef
   | OpShiftLeftLogical of IdResultType * IdResult * Base: IdRef * Shift: IdRef
   | OpBitwiseOr of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpBitwiseXor of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpBitwiseAnd of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpNot of IdResultType * IdResult * Operand: IdRef
   | OpBitFieldInsert of IdResultType * IdResult * Base: IdRef * Insert: IdRef * Offset: IdRef * Count: IdRef
   | OpBitFieldSExtract of IdResultType * IdResult * Base: IdRef * Offset: IdRef * Count: IdRef
   | OpBitFieldUExtract of IdResultType * IdResult * Base: IdRef * Offset: IdRef * Count: IdRef
   | OpBitReverse of IdResultType * IdResult * Base: IdRef
   | OpBitCount of IdResultType * IdResult * Base: IdRef
   | OpDPdx of IdResultType * IdResult * P: IdRef
   | OpDPdy of IdResultType * IdResult * P: IdRef
   | OpFwidth of IdResultType * IdResult * P: IdRef
   | OpDPdxFine of IdResultType * IdResult * P: IdRef
   | OpDPdyFine of IdResultType * IdResult * P: IdRef
   | OpFwidthFine of IdResultType * IdResult * P: IdRef
   | OpDPdxCoarse of IdResultType * IdResult * P: IdRef
   | OpDPdyCoarse of IdResultType * IdResult * P: IdRef
   | OpFwidthCoarse of IdResultType * IdResult * P: IdRef
   | OpEmitVertex
   | OpEndPrimitive
   | OpEmitStreamVertex of Stream: IdRef
   | OpEndStreamPrimitive of Stream: IdRef
   | OpControlBarrier of Execution: IdScope * Memory: IdScope * Semantics: IdMemorySemantics
   | OpMemoryBarrier of Memory: IdScope * Semantics: IdMemorySemantics
   | OpAtomicLoad of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics
   | OpAtomicStore of Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicExchange of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicCompareExchange of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Equal: IdMemorySemantics * Unequal: IdMemorySemantics * Value: IdRef * Comparator: IdRef
   | OpAtomicCompareExchangeWeak of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Equal: IdMemorySemantics * Unequal: IdMemorySemantics * Value: IdRef * Comparator: IdRef
   | OpAtomicIIncrement of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics
   | OpAtomicIDecrement of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics
   | OpAtomicIAdd of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicISub of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicSMin of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicUMin of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicSMax of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicUMax of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicAnd of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicOr of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicXor of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpPhi of IdResultType * IdResult * VariableParent: PairIdRefIdRef list
   | OpLoopMerge of MergeBlock: IdRef * ContinueTarget: IdRef * LoopControl
   | OpSelectionMerge of MergeBlock: IdRef * SelectionControl
   | OpLabel of IdResult
   | OpBranch of TargetLabel: IdRef
   | OpBranchConditional of Condition: IdRef * TrueLabel: IdRef * FalseLabel: IdRef * Branchweights: LiteralInteger list
   | OpSwitch of Selector: IdRef * Default: IdRef * Target: PairLiteralIntegerIdRef list
   | OpKill
   | OpReturn
   | OpReturnValue of Value: IdRef
   | OpUnreachable
   | OpLifetimeStart of Pointer: IdRef * Size: LiteralInteger
   | OpLifetimeStop of Pointer: IdRef * Size: LiteralInteger
   | OpGroupAsyncCopy of IdResultType * IdResult * Execution: IdScope * Destination: IdRef * Source: IdRef * NumElements: IdRef * Stride: IdRef * Event: IdRef
   | OpGroupWaitEvents of Execution: IdScope * NumEvents: IdRef * EventsList: IdRef
   | OpGroupAll of IdResultType * IdResult * Execution: IdScope * Predicate: IdRef
   | OpGroupAny of IdResultType * IdResult * Execution: IdScope * Predicate: IdRef
   | OpGroupBroadcast of IdResultType * IdResult * Execution: IdScope * Value: IdRef * LocalId: IdRef
   | OpGroupIAdd of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupFAdd of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupFMin of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupUMin of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupSMin of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupFMax of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupUMax of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupSMax of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpReadPipe of IdResultType * IdResult * Pipe: IdRef * Pointer: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpWritePipe of IdResultType * IdResult * Pipe: IdRef * Pointer: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpReservedReadPipe of IdResultType * IdResult * Pipe: IdRef * ReserveId: IdRef * Index: IdRef * Pointer: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpReservedWritePipe of IdResultType * IdResult * Pipe: IdRef * ReserveId: IdRef * Index: IdRef * Pointer: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpReserveReadPipePackets of IdResultType * IdResult * Pipe: IdRef * NumPackets: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpReserveWritePipePackets of IdResultType * IdResult * Pipe: IdRef * NumPackets: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpCommitReadPipe of Pipe: IdRef * ReserveId: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpCommitWritePipe of Pipe: IdRef * ReserveId: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpIsValidReserveId of IdResultType * IdResult * ReserveId: IdRef
   | OpGetNumPipePackets of IdResultType * IdResult * Pipe: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpGetMaxPipePackets of IdResultType * IdResult * Pipe: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpGroupReserveReadPipePackets of IdResultType * IdResult * Execution: IdScope * Pipe: IdRef * NumPackets: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpGroupReserveWritePipePackets of IdResultType * IdResult * Execution: IdScope * Pipe: IdRef * NumPackets: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpGroupCommitReadPipe of Execution: IdScope * Pipe: IdRef * ReserveId: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpGroupCommitWritePipe of Execution: IdScope * Pipe: IdRef * ReserveId: IdRef * PacketSize: IdRef * PacketAlignment: IdRef
   | OpEnqueueMarker of IdResultType * IdResult * Queue: IdRef * NumEvents: IdRef * WaitEvents: IdRef * RetEvent: IdRef
   | OpEnqueueKernel of IdResultType * IdResult * Queue: IdRef * Flags: IdRef * NDRange: IdRef * NumEvents: IdRef * WaitEvents: IdRef * RetEvent: IdRef * Invoke: IdRef * Param: IdRef * ParamSize: IdRef * ParamAlign: IdRef * LocalSize: IdRef list
   | OpGetKernelNDrangeSubGroupCount of IdResultType * IdResult * NDRange: IdRef * Invoke: IdRef * Param: IdRef * ParamSize: IdRef * ParamAlign: IdRef
   | OpGetKernelNDrangeMaxSubGroupSize of IdResultType * IdResult * NDRange: IdRef * Invoke: IdRef * Param: IdRef * ParamSize: IdRef * ParamAlign: IdRef
   | OpGetKernelWorkGroupSize of IdResultType * IdResult * Invoke: IdRef * Param: IdRef * ParamSize: IdRef * ParamAlign: IdRef
   | OpGetKernelPreferredWorkGroupSizeMultiple of IdResultType * IdResult * Invoke: IdRef * Param: IdRef * ParamSize: IdRef * ParamAlign: IdRef
   | OpRetainEvent of Event: IdRef
   | OpReleaseEvent of Event: IdRef
   | OpCreateUserEvent of IdResultType * IdResult
   | OpIsValidEvent of IdResultType * IdResult * Event: IdRef
   | OpSetUserEventStatus of Event: IdRef * Status: IdRef
   | OpCaptureEventProfilingInfo of Event: IdRef * ProfilingInfo: IdRef * Value: IdRef
   | OpGetDefaultQueue of IdResultType * IdResult
   | OpBuildNDRange of IdResultType * IdResult * GlobalWorkSize: IdRef * LocalWorkSize: IdRef * GlobalWorkOffset: IdRef
   | OpImageSparseSampleImplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * ImageOperands option
   | OpImageSparseSampleExplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * ImageOperands
   | OpImageSparseSampleDrefImplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Dref: IdRef * ImageOperands option
   | OpImageSparseSampleDrefExplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Dref: IdRef * ImageOperands
   | OpImageSparseSampleProjImplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * ImageOperands option
   | OpImageSparseSampleProjExplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * ImageOperands
   | OpImageSparseSampleProjDrefImplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Dref: IdRef * ImageOperands option
   | OpImageSparseSampleProjDrefExplicitLod of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Dref: IdRef * ImageOperands
   | OpImageSparseFetch of IdResultType * IdResult * Image: IdRef * Coordinate: IdRef * ImageOperands option
   | OpImageSparseGather of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Component: IdRef * ImageOperands option
   | OpImageSparseDrefGather of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Dref: IdRef * ImageOperands option
   | OpImageSparseTexelsResident of IdResultType * IdResult * ResidentCode: IdRef
   | OpNoLine
   | OpAtomicFlagTestAndSet of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics
   | OpAtomicFlagClear of Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics
   | OpImageSparseRead of IdResultType * IdResult * Image: IdRef * Coordinate: IdRef * ImageOperands option
   | OpSizeOf of IdResultType * IdResult * Pointer: IdRef
   | OpTypePipeStorage of IdResult
   | OpConstantPipeStorage of IdResultType * IdResult * PacketSize: LiteralInteger * PacketAlignment: LiteralInteger * Capacity: LiteralInteger
   | OpCreatePipeFromPipeStorage of IdResultType * IdResult * PipeStorage: IdRef
   | OpGetKernelLocalSizeForSubgroupCount of IdResultType * IdResult * SubgroupCount: IdRef * Invoke: IdRef * Param: IdRef * ParamSize: IdRef * ParamAlign: IdRef
   | OpGetKernelMaxNumSubgroups of IdResultType * IdResult * Invoke: IdRef * Param: IdRef * ParamSize: IdRef * ParamAlign: IdRef
   | OpTypeNamedBarrier of IdResult
   | OpNamedBarrierInitialize of IdResultType * IdResult * SubgroupCount: IdRef
   | OpMemoryNamedBarrier of NamedBarrier: IdRef * Memory: IdScope * Semantics: IdMemorySemantics
   | OpModuleProcessed of Process: LiteralString
   | OpExecutionModeId of EntryPoint: IdRef * Mode: ExecutionMode
   | OpDecorateId of Target: IdRef * Decoration
   | OpGroupNonUniformElect of IdResultType * IdResult * Execution: IdScope
   | OpGroupNonUniformAll of IdResultType * IdResult * Execution: IdScope * Predicate: IdRef
   | OpGroupNonUniformAny of IdResultType * IdResult * Execution: IdScope * Predicate: IdRef
   | OpGroupNonUniformAllEqual of IdResultType * IdResult * Execution: IdScope * Value: IdRef
   | OpGroupNonUniformBroadcast of IdResultType * IdResult * Execution: IdScope * Value: IdRef * Id: IdRef
   | OpGroupNonUniformBroadcastFirst of IdResultType * IdResult * Execution: IdScope * Value: IdRef
   | OpGroupNonUniformBallot of IdResultType * IdResult * Execution: IdScope * Predicate: IdRef
   | OpGroupNonUniformInverseBallot of IdResultType * IdResult * Execution: IdScope * Value: IdRef
   | OpGroupNonUniformBallotBitExtract of IdResultType * IdResult * Execution: IdScope * Value: IdRef * Index: IdRef
   | OpGroupNonUniformBallotBitCount of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef
   | OpGroupNonUniformBallotFindLSB of IdResultType * IdResult * Execution: IdScope * Value: IdRef
   | OpGroupNonUniformBallotFindMSB of IdResultType * IdResult * Execution: IdScope * Value: IdRef
   | OpGroupNonUniformShuffle of IdResultType * IdResult * Execution: IdScope * Value: IdRef * Id: IdRef
   | OpGroupNonUniformShuffleXor of IdResultType * IdResult * Execution: IdScope * Value: IdRef * Mask: IdRef
   | OpGroupNonUniformShuffleUp of IdResultType * IdResult * Execution: IdScope * Value: IdRef * Delta: IdRef
   | OpGroupNonUniformShuffleDown of IdResultType * IdResult * Execution: IdScope * Value: IdRef * Delta: IdRef
   | OpGroupNonUniformIAdd of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformFAdd of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformIMul of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformFMul of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformSMin of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformUMin of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformFMin of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformSMax of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformUMax of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformFMax of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformBitwiseAnd of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformBitwiseOr of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformBitwiseXor of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformLogicalAnd of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformLogicalOr of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformLogicalXor of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * Value: IdRef * ClusterSize: IdRef option
   | OpGroupNonUniformQuadBroadcast of IdResultType * IdResult * Execution: IdScope * Value: IdRef * Index: IdRef
   | OpGroupNonUniformQuadSwap of IdResultType * IdResult * Execution: IdScope * Value: IdRef * Direction: IdRef
   | OpCopyLogical of IdResultType * IdResult * Operand: IdRef
   | OpPtrEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpPtrNotEqual of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpPtrDiff of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpColorAttachmentReadEXT of IdResultType * IdResult * Attachment: IdRef * Sample: IdRef option
   | OpDepthAttachmentReadEXT of IdResultType * IdResult * Sample: IdRef option
   | OpStencilAttachmentReadEXT of IdResultType * IdResult * Sample: IdRef option
   | OpTerminateInvocation
   | OpTypeUntypedPointerKHR of IdResult * StorageClass
   | OpUntypedVariableKHR of IdResultType * IdResult * StorageClass * DataType: IdRef option * Initializer: IdRef option
   | OpUntypedAccessChainKHR of IdResultType * IdResult * BaseType: IdRef * Base: IdRef * Indexes: IdRef list
   | OpUntypedInBoundsAccessChainKHR of IdResultType * IdResult * BaseType: IdRef * Base: IdRef * Indexes: IdRef list
   | OpSubgroupBallotKHR of IdResultType * IdResult * Predicate: IdRef
   | OpSubgroupFirstInvocationKHR of IdResultType * IdResult * Value: IdRef
   | OpUntypedPtrAccessChainKHR of IdResultType * IdResult * BaseType: IdRef * Base: IdRef * Element: IdRef * Indexes: IdRef list
   | OpUntypedInBoundsPtrAccessChainKHR of IdResultType * IdResult * BaseType: IdRef * Base: IdRef * Element: IdRef * Indexes: IdRef list
   | OpUntypedArrayLengthKHR of IdResultType * IdResult * Structure: IdRef * Pointer: IdRef * Arraymember: LiteralInteger
   | OpUntypedPrefetchKHR of PointerType: IdRef * NumBytes: IdRef * RW: IdRef option * Locality: IdRef option * CacheType: IdRef option
   | OpSubgroupAllKHR of IdResultType * IdResult * Predicate: IdRef
   | OpSubgroupAnyKHR of IdResultType * IdResult * Predicate: IdRef
   | OpSubgroupAllEqualKHR of IdResultType * IdResult * Predicate: IdRef
   | OpGroupNonUniformRotateKHR of IdResultType * IdResult * Execution: IdScope * Value: IdRef * Delta: IdRef * ClusterSize: IdRef option
   | OpSubgroupReadInvocationKHR of IdResultType * IdResult * Value: IdRef * Index: IdRef
   | OpExtInstWithForwardRefsKHR of IdResultType * IdResult * Set: IdRef * Instruction: LiteralExtInstInteger * Operand1: IdRef list
   | OpTraceRayKHR of Accel: IdRef * RayFlags: IdRef * CullMask: IdRef * SBTOffset: IdRef * SBTStride: IdRef * MissIndex: IdRef * RayOrigin: IdRef * RayTmin: IdRef * RayDirection: IdRef * RayTmax: IdRef * Payload: IdRef
   | OpExecuteCallableKHR of SBTIndex: IdRef * CallableData: IdRef
   | OpConvertUToAccelerationStructureKHR of IdResultType * IdResult * Accel: IdRef
   | OpIgnoreIntersectionKHR
   | OpTerminateRayKHR
   | OpSDot of IdResultType * IdResult * Vector1: IdRef * Vector2: IdRef * PackedVectorFormat: PackedVectorFormat option
   | OpUDot of IdResultType * IdResult * Vector1: IdRef * Vector2: IdRef * PackedVectorFormat: PackedVectorFormat option
   | OpSUDot of IdResultType * IdResult * Vector1: IdRef * Vector2: IdRef * PackedVectorFormat: PackedVectorFormat option
   | OpSDotAccSat of IdResultType * IdResult * Vector1: IdRef * Vector2: IdRef * Accumulator: IdRef * PackedVectorFormat: PackedVectorFormat option
   | OpUDotAccSat of IdResultType * IdResult * Vector1: IdRef * Vector2: IdRef * Accumulator: IdRef * PackedVectorFormat: PackedVectorFormat option
   | OpSUDotAccSat of IdResultType * IdResult * Vector1: IdRef * Vector2: IdRef * Accumulator: IdRef * PackedVectorFormat: PackedVectorFormat option
   | OpTypeCooperativeMatrixKHR of IdResult * ComponentType: IdRef * Scope: IdScope * Rows: IdRef * Columns: IdRef * Use: IdRef
   | OpCooperativeMatrixLoadKHR of IdResultType * IdResult * Pointer: IdRef * MemoryLayout: IdRef * Stride: IdRef option * MemoryOperand: MemoryAccess option
   | OpCooperativeMatrixStoreKHR of Pointer: IdRef * Object: IdRef * MemoryLayout: IdRef * Stride: IdRef option * MemoryOperand: MemoryAccess option
   | OpCooperativeMatrixMulAddKHR of IdResultType * IdResult * A: IdRef * B: IdRef * C: IdRef * CooperativeMatrixOperands: CooperativeMatrixOperands option
   | OpCooperativeMatrixLengthKHR of IdResultType * IdResult * Type: IdRef
   | OpConstantCompositeReplicateEXT of IdResultType * IdResult * Value: IdRef
   | OpSpecConstantCompositeReplicateEXT of IdResultType * IdResult * Value: IdRef
   | OpCompositeConstructReplicateEXT of IdResultType * IdResult * Value: IdRef
   | OpTypeRayQueryKHR of IdResult
   | OpRayQueryInitializeKHR of RayQuery: IdRef * Accel: IdRef * RayFlags: IdRef * CullMask: IdRef * RayOrigin: IdRef * RayTMin: IdRef * RayDirection: IdRef * RayTMax: IdRef
   | OpRayQueryTerminateKHR of RayQuery: IdRef
   | OpRayQueryGenerateIntersectionKHR of RayQuery: IdRef * HitT: IdRef
   | OpRayQueryConfirmIntersectionKHR of RayQuery: IdRef
   | OpRayQueryProceedKHR of IdResultType * IdResult * RayQuery: IdRef
   | OpRayQueryGetIntersectionTypeKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpImageSampleWeightedQCOM of IdResultType * IdResult * Texture: IdRef * Coordinates: IdRef * Weights: IdRef
   | OpImageBoxFilterQCOM of IdResultType * IdResult * Texture: IdRef * Coordinates: IdRef * BoxSize: IdRef
   | OpImageBlockMatchSSDQCOM of IdResultType * IdResult * Target: IdRef * TargetCoordinates: IdRef * Reference: IdRef * ReferenceCoordinates: IdRef * BlockSize: IdRef
   | OpImageBlockMatchSADQCOM of IdResultType * IdResult * Target: IdRef * TargetCoordinates: IdRef * Reference: IdRef * ReferenceCoordinates: IdRef * BlockSize: IdRef
   | OpImageBlockMatchWindowSSDQCOM of IdResultType * IdResult * TargetSampledImage: IdRef * TargetCoordinates: IdRef * ReferenceSampledImage: IdRef * ReferenceCoordinates: IdRef * BlockSize: IdRef
   | OpImageBlockMatchWindowSADQCOM of IdResultType * IdResult * TargetSampledImage: IdRef * TargetCoordinates: IdRef * ReferenceSampledImage: IdRef * ReferenceCoordinates: IdRef * BlockSize: IdRef
   | OpImageBlockMatchGatherSSDQCOM of IdResultType * IdResult * TargetSampledImage: IdRef * TargetCoordinates: IdRef * ReferenceSampledImage: IdRef * ReferenceCoordinates: IdRef * BlockSize: IdRef
   | OpImageBlockMatchGatherSADQCOM of IdResultType * IdResult * TargetSampledImage: IdRef * TargetCoordinates: IdRef * ReferenceSampledImage: IdRef * ReferenceCoordinates: IdRef * BlockSize: IdRef
   | OpGroupIAddNonUniformAMD of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupFAddNonUniformAMD of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupFMinNonUniformAMD of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupUMinNonUniformAMD of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupSMinNonUniformAMD of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupFMaxNonUniformAMD of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupUMaxNonUniformAMD of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupSMaxNonUniformAMD of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpFragmentMaskFetchAMD of IdResultType * IdResult * Image: IdRef * Coordinate: IdRef
   | OpFragmentFetchAMD of IdResultType * IdResult * Image: IdRef * Coordinate: IdRef * FragmentIndex: IdRef
   | OpReadClockKHR of IdResultType * IdResult * Scope: IdScope
   | OpAllocateNodePayloadsAMDX of IdResultType * IdResult * Visibility: IdScope * PayloadCount: IdRef * NodeIndex: IdRef
   | OpEnqueueNodePayloadsAMDX of PayloadArray: IdRef
   | OpTypeNodePayloadArrayAMDX of IdResult * PayloadType: IdRef
   | OpFinishWritingNodePayloadAMDX of IdResultType * IdResult * Payload: IdRef
   | OpNodePayloadArrayLengthAMDX of IdResultType * IdResult * PayloadArray: IdRef
   | OpIsNodePayloadValidAMDX of IdResultType * IdResult * PayloadType: IdRef * NodeIndex: IdRef
   | OpConstantStringAMDX of IdResult * LiteralString: LiteralString
   | OpSpecConstantStringAMDX of IdResult * LiteralString: LiteralString
   | OpGroupNonUniformQuadAllKHR of IdResultType * IdResult * Predicate: IdRef
   | OpGroupNonUniformQuadAnyKHR of IdResultType * IdResult * Predicate: IdRef
   | OpHitObjectRecordHitMotionNV of HitObject: IdRef * AccelerationStructure: IdRef * InstanceId: IdRef * PrimitiveId: IdRef * GeometryIndex: IdRef * HitKind: IdRef * SBTRecordOffset: IdRef * SBTRecordStride: IdRef * Origin: IdRef * TMin: IdRef * Direction: IdRef * TMax: IdRef * CurrentTime: IdRef * HitObjectAttributes: IdRef
   | OpHitObjectRecordHitWithIndexMotionNV of HitObject: IdRef * AccelerationStructure: IdRef * InstanceId: IdRef * PrimitiveId: IdRef * GeometryIndex: IdRef * HitKind: IdRef * SBTRecordIndex: IdRef * Origin: IdRef * TMin: IdRef * Direction: IdRef * TMax: IdRef * CurrentTime: IdRef * HitObjectAttributes: IdRef
   | OpHitObjectRecordMissMotionNV of HitObject: IdRef * SBTIndex: IdRef * Origin: IdRef * TMin: IdRef * Direction: IdRef * TMax: IdRef * CurrentTime: IdRef
   | OpHitObjectGetWorldToObjectNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetObjectToWorldNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetObjectRayDirectionNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetObjectRayOriginNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectTraceRayMotionNV of HitObject: IdRef * AccelerationStructure: IdRef * RayFlags: IdRef * Cullmask: IdRef * SBTRecordOffset: IdRef * SBTRecordStride: IdRef * MissIndex: IdRef * Origin: IdRef * TMin: IdRef * Direction: IdRef * TMax: IdRef * Time: IdRef * Payload: IdRef
   | OpHitObjectGetShaderRecordBufferHandleNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetShaderBindingTableRecordIndexNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectRecordEmptyNV of HitObject: IdRef
   | OpHitObjectTraceRayNV of HitObject: IdRef * AccelerationStructure: IdRef * RayFlags: IdRef * Cullmask: IdRef * SBTRecordOffset: IdRef * SBTRecordStride: IdRef * MissIndex: IdRef * Origin: IdRef * TMin: IdRef * Direction: IdRef * TMax: IdRef * Payload: IdRef
   | OpHitObjectRecordHitNV of HitObject: IdRef * AccelerationStructure: IdRef * InstanceId: IdRef * PrimitiveId: IdRef * GeometryIndex: IdRef * HitKind: IdRef * SBTRecordOffset: IdRef * SBTRecordStride: IdRef * Origin: IdRef * TMin: IdRef * Direction: IdRef * TMax: IdRef * HitObjectAttributes: IdRef
   | OpHitObjectRecordHitWithIndexNV of HitObject: IdRef * AccelerationStructure: IdRef * InstanceId: IdRef * PrimitiveId: IdRef * GeometryIndex: IdRef * HitKind: IdRef * SBTRecordIndex: IdRef * Origin: IdRef * TMin: IdRef * Direction: IdRef * TMax: IdRef * HitObjectAttributes: IdRef
   | OpHitObjectRecordMissNV of HitObject: IdRef * SBTIndex: IdRef * Origin: IdRef * TMin: IdRef * Direction: IdRef * TMax: IdRef
   | OpHitObjectExecuteShaderNV of HitObject: IdRef * Payload: IdRef
   | OpHitObjectGetCurrentTimeNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetAttributesNV of HitObject: IdRef * HitObjectAttribute: IdRef
   | OpHitObjectGetHitKindNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetPrimitiveIndexNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetGeometryIndexNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetInstanceIdNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetInstanceCustomIndexNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetWorldRayDirectionNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetWorldRayOriginNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetRayTMaxNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectGetRayTMinNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectIsEmptyNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectIsHitNV of IdResultType * IdResult * HitObject: IdRef
   | OpHitObjectIsMissNV of IdResultType * IdResult * HitObject: IdRef
   | OpReorderThreadWithHitObjectNV of HitObject: IdRef * Hint: IdRef option * Bits: IdRef option
   | OpReorderThreadWithHintNV of Hint: IdRef * Bits: IdRef
   | OpTypeHitObjectNV of IdResult
   | OpImageSampleFootprintNV of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Granularity: IdRef * Coarse: IdRef * ImageOperands option
   | OpCooperativeMatrixConvertNV of IdResultType * IdResult * Matrix: IdRef
   | OpEmitMeshTasksEXT of GroupCountX: IdRef * GroupCountY: IdRef * GroupCountZ: IdRef * Payload: IdRef option
   | OpSetMeshOutputsEXT of VertexCount: IdRef * PrimitiveCount: IdRef
   | OpGroupNonUniformPartitionNV of IdResultType * IdResult * Value: IdRef
   | OpWritePackedPrimitiveIndices4x8NV of IndexOffset: IdRef * PackedIndices: IdRef
   | OpFetchMicroTriangleVertexPositionNV of IdResultType * IdResult * Accel: IdRef * InstanceId: IdRef * GeometryIndex: IdRef * PrimitiveIndex: IdRef * Barycentric: IdRef
   | OpFetchMicroTriangleVertexBarycentricNV of IdResultType * IdResult * Accel: IdRef * InstanceId: IdRef * GeometryIndex: IdRef * PrimitiveIndex: IdRef * Barycentric: IdRef
   | OpReportIntersectionKHR of IdResultType * IdResult * Hit: IdRef * HitKind: IdRef
   | OpIgnoreIntersectionNV
   | OpTerminateRayNV
   | OpTraceNV of Accel: IdRef * RayFlags: IdRef * CullMask: IdRef * SBTOffset: IdRef * SBTStride: IdRef * MissIndex: IdRef * RayOrigin: IdRef * RayTmin: IdRef * RayDirection: IdRef * RayTmax: IdRef * PayloadId: IdRef
   | OpTraceMotionNV of Accel: IdRef * RayFlags: IdRef * CullMask: IdRef * SBTOffset: IdRef * SBTStride: IdRef * MissIndex: IdRef * RayOrigin: IdRef * RayTmin: IdRef * RayDirection: IdRef * RayTmax: IdRef * Time: IdRef * PayloadId: IdRef
   | OpTraceRayMotionNV of Accel: IdRef * RayFlags: IdRef * CullMask: IdRef * SBTOffset: IdRef * SBTStride: IdRef * MissIndex: IdRef * RayOrigin: IdRef * RayTmin: IdRef * RayDirection: IdRef * RayTmax: IdRef * Time: IdRef * Payload: IdRef
   | OpRayQueryGetIntersectionTriangleVertexPositionsKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpTypeAccelerationStructureKHR of IdResult
   | OpExecuteCallableNV of SBTIndex: IdRef * CallableDataId: IdRef
   | OpTypeCooperativeMatrixNV of IdResult * ComponentType: IdRef * Execution: IdScope * Rows: IdRef * Columns: IdRef
   | OpCooperativeMatrixLoadNV of IdResultType * IdResult * Pointer: IdRef * Stride: IdRef * ColumnMajor: IdRef * MemoryAccess option
   | OpCooperativeMatrixStoreNV of Pointer: IdRef * Object: IdRef * Stride: IdRef * ColumnMajor: IdRef * MemoryAccess option
   | OpCooperativeMatrixMulAddNV of IdResultType * IdResult * A: IdRef * B: IdRef * C: IdRef
   | OpCooperativeMatrixLengthNV of IdResultType * IdResult * Type: IdRef
   | OpBeginInvocationInterlockEXT
   | OpEndInvocationInterlockEXT
   | OpCooperativeMatrixReduceNV of IdResultType * IdResult * Matrix: IdRef * Reduce: CooperativeMatrixReduce * CombineFunc: IdRef
   | OpCooperativeMatrixLoadTensorNV of IdResultType * IdResult * Pointer: IdRef * Object: IdRef * TensorLayout: IdRef * MemoryOperand: MemoryAccess * TensorAddressingOperands: TensorAddressingOperands
   | OpCooperativeMatrixStoreTensorNV of Pointer: IdRef * Object: IdRef * TensorLayout: IdRef * MemoryOperand: MemoryAccess * TensorAddressingOperands: TensorAddressingOperands
   | OpCooperativeMatrixPerElementOpNV of IdResultType * IdResult * Matrix: IdRef * Func: IdRef * Operands: IdRef list
   | OpTypeTensorLayoutNV of IdResult * Dim: IdRef * ClampMode: IdRef
   | OpTypeTensorViewNV of IdResult * Dim: IdRef * HasDimensions: IdRef * p: IdRef list
   | OpCreateTensorLayoutNV of IdResultType * IdResult
   | OpTensorLayoutSetDimensionNV of IdResultType * IdResult * TensorLayout: IdRef * Dim: IdRef list
   | OpTensorLayoutSetStrideNV of IdResultType * IdResult * TensorLayout: IdRef * Stride: IdRef list
   | OpTensorLayoutSliceNV of IdResultType * IdResult * TensorLayout: IdRef * Operands: IdRef list
   | OpTensorLayoutSetClampValueNV of IdResultType * IdResult * TensorLayout: IdRef * Value: IdRef
   | OpCreateTensorViewNV of IdResultType * IdResult
   | OpTensorViewSetDimensionNV of IdResultType * IdResult * TensorView: IdRef * Dim: IdRef list
   | OpTensorViewSetStrideNV of IdResultType * IdResult * TensorView: IdRef * Stride: IdRef list
   | OpDemoteToHelperInvocation
   | OpIsHelperInvocationEXT of IdResultType * IdResult
   | OpTensorViewSetClipNV of IdResultType * IdResult * TensorView: IdRef * ClipRowOffset: IdRef * ClipRowSpan: IdRef * ClipColOffset: IdRef * ClipColSpan: IdRef
   | OpTensorLayoutSetBlockSizeNV of IdResultType * IdResult * TensorLayout: IdRef * BlockSize: IdRef list
   | OpCooperativeMatrixTransposeNV of IdResultType * IdResult * Matrix: IdRef
   | OpConvertUToImageNV of IdResultType * IdResult * Operand: IdRef
   | OpConvertUToSamplerNV of IdResultType * IdResult * Operand: IdRef
   | OpConvertImageToUNV of IdResultType * IdResult * Operand: IdRef
   | OpConvertSamplerToUNV of IdResultType * IdResult * Operand: IdRef
   | OpConvertUToSampledImageNV of IdResultType * IdResult * Operand: IdRef
   | OpConvertSampledImageToUNV of IdResultType * IdResult * Operand: IdRef
   | OpSamplerImageAddressingModeNV of BitWidth: LiteralInteger
   | OpRawAccessChainNV of IdResultType * IdResult * Base: IdRef * Bytestride: IdRef * Elementindex: IdRef * Byteoffset: IdRef * RawAccessChainOperands option
   | OpSubgroupShuffleINTEL of IdResultType * IdResult * Data: IdRef * InvocationId: IdRef
   | OpSubgroupShuffleDownINTEL of IdResultType * IdResult * Current: IdRef * Next: IdRef * Delta: IdRef
   | OpSubgroupShuffleUpINTEL of IdResultType * IdResult * Previous: IdRef * Current: IdRef * Delta: IdRef
   | OpSubgroupShuffleXorINTEL of IdResultType * IdResult * Data: IdRef * Value: IdRef
   | OpSubgroupBlockReadINTEL of IdResultType * IdResult * Ptr: IdRef
   | OpSubgroupBlockWriteINTEL of Ptr: IdRef * Data: IdRef
   | OpSubgroupImageBlockReadINTEL of IdResultType * IdResult * Image: IdRef * Coordinate: IdRef
   | OpSubgroupImageBlockWriteINTEL of Image: IdRef * Coordinate: IdRef * Data: IdRef
   | OpSubgroupImageMediaBlockReadINTEL of IdResultType * IdResult * Image: IdRef * Coordinate: IdRef * Width: IdRef * Height: IdRef
   | OpSubgroupImageMediaBlockWriteINTEL of Image: IdRef * Coordinate: IdRef * Width: IdRef * Height: IdRef * Data: IdRef
   | OpUCountLeadingZerosINTEL of IdResultType * IdResult * Operand: IdRef
   | OpUCountTrailingZerosINTEL of IdResultType * IdResult * Operand: IdRef
   | OpAbsISubINTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpAbsUSubINTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpIAddSatINTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpUAddSatINTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpIAverageINTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpUAverageINTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpIAverageRoundedINTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpUAverageRoundedINTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpISubSatINTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpUSubSatINTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpIMul32x16INTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpUMul32x16INTEL of IdResultType * IdResult * Operand1: IdRef * Operand2: IdRef
   | OpConstantFunctionPointerINTEL of IdResultType * IdResult * Function: IdRef
   | OpFunctionPointerCallINTEL of IdResultType * IdResult * Operand1: IdRef list
   | OpAsmTargetINTEL of IdResultType * IdResult * Asmtarget: LiteralString
   | OpAsmINTEL of IdResultType * IdResult * Asmtype: IdRef * Target: IdRef * Asminstructions: LiteralString * Constraints: LiteralString
   | OpAsmCallINTEL of IdResultType * IdResult * Asm: IdRef * Argument0: IdRef list
   | OpAtomicFMinEXT of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAtomicFMaxEXT of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpAssumeTrueKHR of Condition: IdRef
   | OpExpectKHR of IdResultType * IdResult * Value: IdRef * ExpectedValue: IdRef
   | OpDecorateString of Target: IdRef * Decoration
   | OpMemberDecorateString of StructType: IdRef * Member: LiteralInteger * Decoration
   | OpVmeImageINTEL of IdResultType * IdResult * ImageType: IdRef * Sampler: IdRef
   | OpTypeVmeImageINTEL of IdResult * ImageType: IdRef
   | OpTypeAvcImePayloadINTEL of IdResult
   | OpTypeAvcRefPayloadINTEL of IdResult
   | OpTypeAvcSicPayloadINTEL of IdResult
   | OpTypeAvcMcePayloadINTEL of IdResult
   | OpTypeAvcMceResultINTEL of IdResult
   | OpTypeAvcImeResultINTEL of IdResult
   | OpTypeAvcImeResultSingleReferenceStreamoutINTEL of IdResult
   | OpTypeAvcImeResultDualReferenceStreamoutINTEL of IdResult
   | OpTypeAvcImeSingleReferenceStreaminINTEL of IdResult
   | OpTypeAvcImeDualReferenceStreaminINTEL of IdResult
   | OpTypeAvcRefResultINTEL of IdResult
   | OpTypeAvcSicResultINTEL of IdResult
   | OpSubgroupAvcMceGetDefaultInterBaseMultiReferencePenaltyINTEL of IdResultType * IdResult * SliceType: IdRef * Qp: IdRef
   | OpSubgroupAvcMceSetInterBaseMultiReferencePenaltyINTEL of IdResultType * IdResult * ReferenceBasePenalty: IdRef * Payload: IdRef
   | OpSubgroupAvcMceGetDefaultInterShapePenaltyINTEL of IdResultType * IdResult * SliceType: IdRef * Qp: IdRef
   | OpSubgroupAvcMceSetInterShapePenaltyINTEL of IdResultType * IdResult * PackedShapePenalty: IdRef * Payload: IdRef
   | OpSubgroupAvcMceGetDefaultInterDirectionPenaltyINTEL of IdResultType * IdResult * SliceType: IdRef * Qp: IdRef
   | OpSubgroupAvcMceSetInterDirectionPenaltyINTEL of IdResultType * IdResult * DirectionCost: IdRef * Payload: IdRef
   | OpSubgroupAvcMceGetDefaultIntraLumaShapePenaltyINTEL of IdResultType * IdResult * SliceType: IdRef * Qp: IdRef
   | OpSubgroupAvcMceGetDefaultInterMotionVectorCostTableINTEL of IdResultType * IdResult * SliceType: IdRef * Qp: IdRef
   | OpSubgroupAvcMceGetDefaultHighPenaltyCostTableINTEL of IdResultType * IdResult
   | OpSubgroupAvcMceGetDefaultMediumPenaltyCostTableINTEL of IdResultType * IdResult
   | OpSubgroupAvcMceGetDefaultLowPenaltyCostTableINTEL of IdResultType * IdResult
   | OpSubgroupAvcMceSetMotionVectorCostFunctionINTEL of IdResultType * IdResult * PackedCostCenterDelta: IdRef * PackedCostTable: IdRef * CostPrecision: IdRef * Payload: IdRef
   | OpSubgroupAvcMceGetDefaultIntraLumaModePenaltyINTEL of IdResultType * IdResult * SliceType: IdRef * Qp: IdRef
   | OpSubgroupAvcMceGetDefaultNonDcLumaIntraPenaltyINTEL of IdResultType * IdResult
   | OpSubgroupAvcMceGetDefaultIntraChromaModeBasePenaltyINTEL of IdResultType * IdResult
   | OpSubgroupAvcMceSetAcOnlyHaarINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceSetSourceInterlacedFieldPolarityINTEL of IdResultType * IdResult * SourceFieldPolarity: IdRef * Payload: IdRef
   | OpSubgroupAvcMceSetSingleReferenceInterlacedFieldPolarityINTEL of IdResultType * IdResult * ReferenceFieldPolarity: IdRef * Payload: IdRef
   | OpSubgroupAvcMceSetDualReferenceInterlacedFieldPolaritiesINTEL of IdResultType * IdResult * ForwardReferenceFieldPolarity: IdRef * BackwardReferenceFieldPolarity: IdRef * Payload: IdRef
   | OpSubgroupAvcMceConvertToImePayloadINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceConvertToImeResultINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceConvertToRefPayloadINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceConvertToRefResultINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceConvertToSicPayloadINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceConvertToSicResultINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceGetMotionVectorsINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceGetInterDistortionsINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceGetBestInterDistortionsINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceGetInterMajorShapeINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceGetInterMinorShapeINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceGetInterDirectionsINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceGetInterMotionVectorCountINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceGetInterReferenceIdsINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcMceGetInterReferenceInterlacedFieldPolaritiesINTEL of IdResultType * IdResult * PackedReferenceIds: IdRef * PackedReferenceParameterFieldPolarities: IdRef * Payload: IdRef
   | OpSubgroupAvcImeInitializeINTEL of IdResultType * IdResult * SrcCoord: IdRef * PartitionMask: IdRef * SADAdjustment: IdRef
   | OpSubgroupAvcImeSetSingleReferenceINTEL of IdResultType * IdResult * RefOffset: IdRef * SearchWindowConfig: IdRef * Payload: IdRef
   | OpSubgroupAvcImeSetDualReferenceINTEL of IdResultType * IdResult * FwdRefOffset: IdRef * BwdRefOffset: IdRef * idSearchWindowConfig: IdRef * Payload: IdRef
   | OpSubgroupAvcImeRefWindowSizeINTEL of IdResultType * IdResult * SearchWindowConfig: IdRef * DualRef: IdRef
   | OpSubgroupAvcImeAdjustRefOffsetINTEL of IdResultType * IdResult * RefOffset: IdRef * SrcCoord: IdRef * RefWindowSize: IdRef * ImageSize: IdRef
   | OpSubgroupAvcImeConvertToMcePayloadINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcImeSetMaxMotionVectorCountINTEL of IdResultType * IdResult * MaxMotionVectorCount: IdRef * Payload: IdRef
   | OpSubgroupAvcImeSetUnidirectionalMixDisableINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcImeSetEarlySearchTerminationThresholdINTEL of IdResultType * IdResult * Threshold: IdRef * Payload: IdRef
   | OpSubgroupAvcImeSetWeightedSadINTEL of IdResultType * IdResult * PackedSadWeights: IdRef * Payload: IdRef
   | OpSubgroupAvcImeEvaluateWithSingleReferenceINTEL of IdResultType * IdResult * SrcImage: IdRef * RefImage: IdRef * Payload: IdRef
   | OpSubgroupAvcImeEvaluateWithDualReferenceINTEL of IdResultType * IdResult * SrcImage: IdRef * FwdRefImage: IdRef * BwdRefImage: IdRef * Payload: IdRef
   | OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminINTEL of IdResultType * IdResult * SrcImage: IdRef * RefImage: IdRef * Payload: IdRef * StreaminComponents: IdRef
   | OpSubgroupAvcImeEvaluateWithDualReferenceStreaminINTEL of IdResultType * IdResult * SrcImage: IdRef * FwdRefImage: IdRef * BwdRefImage: IdRef * Payload: IdRef * StreaminComponents: IdRef
   | OpSubgroupAvcImeEvaluateWithSingleReferenceStreamoutINTEL of IdResultType * IdResult * SrcImage: IdRef * RefImage: IdRef * Payload: IdRef
   | OpSubgroupAvcImeEvaluateWithDualReferenceStreamoutINTEL of IdResultType * IdResult * SrcImage: IdRef * FwdRefImage: IdRef * BwdRefImage: IdRef * Payload: IdRef
   | OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminoutINTEL of IdResultType * IdResult * SrcImage: IdRef * RefImage: IdRef * Payload: IdRef * StreaminComponents: IdRef
   | OpSubgroupAvcImeEvaluateWithDualReferenceStreaminoutINTEL of IdResultType * IdResult * SrcImage: IdRef * FwdRefImage: IdRef * BwdRefImage: IdRef * Payload: IdRef * StreaminComponents: IdRef
   | OpSubgroupAvcImeConvertToMceResultINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcImeGetSingleReferenceStreaminINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcImeGetDualReferenceStreaminINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcImeStripSingleReferenceStreamoutINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcImeStripDualReferenceStreamoutINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeMotionVectorsINTEL of IdResultType * IdResult * Payload: IdRef * MajorShape: IdRef
   | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeDistortionsINTEL of IdResultType * IdResult * Payload: IdRef * MajorShape: IdRef
   | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeReferenceIdsINTEL of IdResultType * IdResult * Payload: IdRef * MajorShape: IdRef
   | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeMotionVectorsINTEL of IdResultType * IdResult * Payload: IdRef * MajorShape: IdRef * Direction: IdRef
   | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeDistortionsINTEL of IdResultType * IdResult * Payload: IdRef * MajorShape: IdRef * Direction: IdRef
   | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeReferenceIdsINTEL of IdResultType * IdResult * Payload: IdRef * MajorShape: IdRef * Direction: IdRef
   | OpSubgroupAvcImeGetBorderReachedINTEL of IdResultType * IdResult * ImageSelect: IdRef * Payload: IdRef
   | OpSubgroupAvcImeGetTruncatedSearchIndicationINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcImeGetUnidirectionalEarlySearchTerminationINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcImeGetWeightingPatternMinimumMotionVectorINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcImeGetWeightingPatternMinimumDistortionINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcFmeInitializeINTEL of IdResultType * IdResult * SrcCoord: IdRef * MotionVectors: IdRef * MajorShapes: IdRef * MinorShapes: IdRef * Direction: IdRef * PixelResolution: IdRef * SadAdjustment: IdRef
   | OpSubgroupAvcBmeInitializeINTEL of IdResultType * IdResult * SrcCoord: IdRef * MotionVectors: IdRef * MajorShapes: IdRef * MinorShapes: IdRef * Direction: IdRef * PixelResolution: IdRef * BidirectionalWeight: IdRef * SadAdjustment: IdRef
   | OpSubgroupAvcRefConvertToMcePayloadINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcRefSetBidirectionalMixDisableINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcRefSetBilinearFilterEnableINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcRefEvaluateWithSingleReferenceINTEL of IdResultType * IdResult * SrcImage: IdRef * RefImage: IdRef * Payload: IdRef
   | OpSubgroupAvcRefEvaluateWithDualReferenceINTEL of IdResultType * IdResult * SrcImage: IdRef * FwdRefImage: IdRef * BwdRefImage: IdRef * Payload: IdRef
   | OpSubgroupAvcRefEvaluateWithMultiReferenceINTEL of IdResultType * IdResult * SrcImage: IdRef * PackedReferenceIds: IdRef * Payload: IdRef
   | OpSubgroupAvcRefEvaluateWithMultiReferenceInterlacedINTEL of IdResultType * IdResult * SrcImage: IdRef * PackedReferenceIds: IdRef * PackedReferenceFieldPolarities: IdRef * Payload: IdRef
   | OpSubgroupAvcRefConvertToMceResultINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicInitializeINTEL of IdResultType * IdResult * SrcCoord: IdRef
   | OpSubgroupAvcSicConfigureSkcINTEL of IdResultType * IdResult * SkipBlockPartitionType: IdRef * SkipMotionVectorMask: IdRef * MotionVectors: IdRef * BidirectionalWeight: IdRef * SadAdjustment: IdRef * Payload: IdRef
   | OpSubgroupAvcSicConfigureIpeLumaINTEL of IdResultType * IdResult * LumaIntraPartitionMask: IdRef * IntraNeighbourAvailabilty: IdRef * LeftEdgeLumaPixels: IdRef * UpperLeftCornerLumaPixel: IdRef * UpperEdgeLumaPixels: IdRef * UpperRightEdgeLumaPixels: IdRef * SadAdjustment: IdRef * Payload: IdRef
   | OpSubgroupAvcSicConfigureIpeLumaChromaINTEL of IdResultType * IdResult * LumaIntraPartitionMask: IdRef * IntraNeighbourAvailabilty: IdRef * LeftEdgeLumaPixels: IdRef * UpperLeftCornerLumaPixel: IdRef * UpperEdgeLumaPixels: IdRef * UpperRightEdgeLumaPixels: IdRef * LeftEdgeChromaPixels: IdRef * UpperLeftCornerChromaPixel: IdRef * UpperEdgeChromaPixels: IdRef * SadAdjustment: IdRef * Payload: IdRef
   | OpSubgroupAvcSicGetMotionVectorMaskINTEL of IdResultType * IdResult * SkipBlockPartitionType: IdRef * Direction: IdRef
   | OpSubgroupAvcSicConvertToMcePayloadINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicSetIntraLumaShapePenaltyINTEL of IdResultType * IdResult * PackedShapePenalty: IdRef * Payload: IdRef
   | OpSubgroupAvcSicSetIntraLumaModeCostFunctionINTEL of IdResultType * IdResult * LumaModePenalty: IdRef * LumaPackedNeighborModes: IdRef * LumaPackedNonDcPenalty: IdRef * Payload: IdRef
   | OpSubgroupAvcSicSetIntraChromaModeCostFunctionINTEL of IdResultType * IdResult * ChromaModeBasePenalty: IdRef * Payload: IdRef
   | OpSubgroupAvcSicSetBilinearFilterEnableINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicSetSkcForwardTransformEnableINTEL of IdResultType * IdResult * PackedSadCoefficients: IdRef * Payload: IdRef
   | OpSubgroupAvcSicSetBlockBasedRawSkipSadINTEL of IdResultType * IdResult * BlockBasedSkipType: IdRef * Payload: IdRef
   | OpSubgroupAvcSicEvaluateIpeINTEL of IdResultType * IdResult * SrcImage: IdRef * Payload: IdRef
   | OpSubgroupAvcSicEvaluateWithSingleReferenceINTEL of IdResultType * IdResult * SrcImage: IdRef * RefImage: IdRef * Payload: IdRef
   | OpSubgroupAvcSicEvaluateWithDualReferenceINTEL of IdResultType * IdResult * SrcImage: IdRef * FwdRefImage: IdRef * BwdRefImage: IdRef * Payload: IdRef
   | OpSubgroupAvcSicEvaluateWithMultiReferenceINTEL of IdResultType * IdResult * SrcImage: IdRef * PackedReferenceIds: IdRef * Payload: IdRef
   | OpSubgroupAvcSicEvaluateWithMultiReferenceInterlacedINTEL of IdResultType * IdResult * SrcImage: IdRef * PackedReferenceIds: IdRef * PackedReferenceFieldPolarities: IdRef * Payload: IdRef
   | OpSubgroupAvcSicConvertToMceResultINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicGetIpeLumaShapeINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicGetBestIpeLumaDistortionINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicGetBestIpeChromaDistortionINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicGetPackedIpeLumaModesINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicGetIpeChromaModeINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicGetPackedSkcLumaCountThresholdINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicGetPackedSkcLumaSumThresholdINTEL of IdResultType * IdResult * Payload: IdRef
   | OpSubgroupAvcSicGetInterRawSadsINTEL of IdResultType * IdResult * Payload: IdRef
   | OpVariableLengthArrayINTEL of IdResultType * IdResult * Lenght: IdRef
   | OpSaveMemoryINTEL of IdResultType * IdResult
   | OpRestoreMemoryINTEL of Ptr: IdRef
   | OpArbitraryFloatSinCosPiINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * FromSign: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatCastINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatCastFromIntINTEL of IdResultType * IdResult * A: IdRef * Mout: LiteralInteger * FromSign: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatCastToIntINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatAddINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatSubINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatMulINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatDivINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatGTINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger
   | OpArbitraryFloatGEINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger
   | OpArbitraryFloatLTINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger
   | OpArbitraryFloatLEINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger
   | OpArbitraryFloatEQINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger
   | OpArbitraryFloatRecipINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatRSqrtINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatCbrtINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatHypotINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatSqrtINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatLogINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatLog2INTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatLog10INTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatLog1pINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatExpINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatExp2INTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatExp10INTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatExpm1INTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatSinINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatCosINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatSinCosINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatSinPiINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatCosPiINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatASinINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatASinPiINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatACosINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatACosPiINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatATanINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatATanPiINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatATan2INTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatPowINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatPowRINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * M2: LiteralInteger * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpArbitraryFloatPowNINTEL of IdResultType * IdResult * A: IdRef * M1: LiteralInteger * B: IdRef * Mout: LiteralInteger * EnableSubnormals: LiteralInteger * RoundingMode: LiteralInteger * RoundingAccuracy: LiteralInteger
   | OpLoopControlINTEL of LoopControlParameters: LiteralInteger list
   | OpAliasDomainDeclINTEL of IdResult * Name: IdRef option
   | OpAliasScopeDeclINTEL of IdResult * AliasDomain: IdRef * Name: IdRef option
   | OpAliasScopeListDeclINTEL of IdResult * AliasScope1AliasScope2: IdRef list
   | OpFixedSqrtINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpFixedRecipINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpFixedRsqrtINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpFixedSinINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpFixedCosINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpFixedSinCosINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpFixedSinPiINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpFixedCosPiINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpFixedSinCosPiINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpFixedLogINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpFixedExpINTEL of IdResultType * IdResult * InputType: IdRef * Input: IdRef * S: LiteralInteger * I: LiteralInteger * rI: LiteralInteger * Q: LiteralInteger * O: LiteralInteger
   | OpPtrCastToCrossWorkgroupINTEL of IdResultType * IdResult * Pointer: IdRef
   | OpCrossWorkgroupCastToPtrINTEL of IdResultType * IdResult * Pointer: IdRef
   | OpReadPipeBlockingINTEL of IdResultType * IdResult * PacketSize: IdRef * PacketAlignment: IdRef
   | OpWritePipeBlockingINTEL of IdResultType * IdResult * PacketSize: IdRef * PacketAlignment: IdRef
   | OpFPGARegINTEL of IdResultType * IdResult * Result: IdRef * Input: IdRef
   | OpRayQueryGetRayTMinKHR of IdResultType * IdResult * RayQuery: IdRef
   | OpRayQueryGetRayFlagsKHR of IdResultType * IdResult * RayQuery: IdRef
   | OpRayQueryGetIntersectionTKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetIntersectionInstanceCustomIndexKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetIntersectionInstanceIdKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetIntersectionGeometryIndexKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetIntersectionPrimitiveIndexKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetIntersectionBarycentricsKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetIntersectionFrontFaceKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetIntersectionCandidateAABBOpaqueKHR of IdResultType * IdResult * RayQuery: IdRef
   | OpRayQueryGetIntersectionObjectRayDirectionKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetIntersectionObjectRayOriginKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetWorldRayDirectionKHR of IdResultType * IdResult * RayQuery: IdRef
   | OpRayQueryGetWorldRayOriginKHR of IdResultType * IdResult * RayQuery: IdRef
   | OpRayQueryGetIntersectionObjectToWorldKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpRayQueryGetIntersectionWorldToObjectKHR of IdResultType * IdResult * RayQuery: IdRef * Intersection: IdRef
   | OpAtomicFAddEXT of IdResultType * IdResult * Pointer: IdRef * Memory: IdScope * Semantics: IdMemorySemantics * Value: IdRef
   | OpTypeBufferSurfaceINTEL of IdResult * AccessQualifier: AccessQualifier
   | OpTypeStructContinuedINTEL of Member0type: IdRef list
   | OpConstantCompositeContinuedINTEL of Constituents: IdRef list
   | OpSpecConstantCompositeContinuedINTEL of Constituents: IdRef list
   | OpCompositeConstructContinuedINTEL of IdResultType * IdResult * Constituents: IdRef list
   | OpConvertFToBF16INTEL of IdResultType * IdResult * FloatValue: IdRef
   | OpConvertBF16ToFINTEL of IdResultType * IdResult * BFloat16Value: IdRef
   | OpControlBarrierArriveINTEL of Execution: IdScope * Memory: IdScope * Semantics: IdMemorySemantics
   | OpControlBarrierWaitINTEL of Execution: IdScope * Memory: IdScope * Semantics: IdMemorySemantics
   | OpArithmeticFenceEXT of IdResultType * IdResult * Target: IdRef
   | OpSubgroupBlockPrefetchINTEL of Ptr: IdRef * NumBytes: IdRef * MemoryAccess option
   | OpGroupIMulKHR of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupFMulKHR of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupBitwiseAndKHR of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupBitwiseOrKHR of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupBitwiseXorKHR of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupLogicalAndKHR of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupLogicalOrKHR of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpGroupLogicalXorKHR of IdResultType * IdResult * Execution: IdScope * Operation: GroupOperation * X: IdRef
   | OpMaskedGatherINTEL of IdResultType * IdResult * PtrVector: IdRef * Alignment: LiteralInteger * Mask: IdRef * FillEmpty: IdRef
   | OpMaskedScatterINTEL of InputVector: IdRef * PtrVector: IdRef * Alignment: LiteralInteger * Mask: IdRef

    member x.Opcode =
       match x with
       | OpNop -> 0us
       | OpUndef _ -> 1us
       | OpSourceContinued _ -> 2us
       | OpSource _ -> 3us
       | OpSourceExtension _ -> 4us
       | OpName _ -> 5us
       | OpMemberName _ -> 6us
       | OpString _ -> 7us
       | OpLine _ -> 8us
       | OpExtension _ -> 10us
       | OpExtInstImport _ -> 11us
       | OpExtInst _ -> 12us
       | OpMemoryModel _ -> 14us
       | OpEntryPoint _ -> 15us
       | OpExecutionMode _ -> 16us
       | OpCapability _ -> 17us
       | OpTypeVoid _ -> 19us
       | OpTypeBool _ -> 20us
       | OpTypeInt _ -> 21us
       | OpTypeFloat _ -> 22us
       | OpTypeVector _ -> 23us
       | OpTypeMatrix _ -> 24us
       | OpTypeImage _ -> 25us
       | OpTypeSampler _ -> 26us
       | OpTypeSampledImage _ -> 27us
       | OpTypeArray _ -> 28us
       | OpTypeRuntimeArray _ -> 29us
       | OpTypeStruct _ -> 30us
       | OpTypeOpaque _ -> 31us
       | OpTypePointer _ -> 32us
       | OpTypeFunction _ -> 33us
       | OpTypeEvent _ -> 34us
       | OpTypeDeviceEvent _ -> 35us
       | OpTypeReserveId _ -> 36us
       | OpTypeQueue _ -> 37us
       | OpTypePipe _ -> 38us
       | OpTypeForwardPointer _ -> 39us
       | OpConstantTrue _ -> 41us
       | OpConstantFalse _ -> 42us
       | OpConstant _ -> 43us
       | OpConstantComposite _ -> 44us
       | OpConstantSampler _ -> 45us
       | OpConstantNull _ -> 46us
       | OpSpecConstantTrue _ -> 48us
       | OpSpecConstantFalse _ -> 49us
       | OpSpecConstant _ -> 50us
       | OpSpecConstantComposite _ -> 51us
       | OpSpecConstantOp _ -> 52us
       | OpFunction _ -> 54us
       | OpFunctionParameter _ -> 55us
       | OpFunctionEnd -> 56us
       | OpFunctionCall _ -> 57us
       | OpVariable _ -> 59us
       | OpImageTexelPointer _ -> 60us
       | OpLoad _ -> 61us
       | OpStore _ -> 62us
       | OpCopyMemory _ -> 63us
       | OpCopyMemorySized _ -> 64us
       | OpAccessChain _ -> 65us
       | OpInBoundsAccessChain _ -> 66us
       | OpPtrAccessChain _ -> 67us
       | OpArrayLength _ -> 68us
       | OpGenericPtrMemSemantics _ -> 69us
       | OpInBoundsPtrAccessChain _ -> 70us
       | OpDecorate _ -> 71us
       | OpMemberDecorate _ -> 72us
       | OpDecorationGroup _ -> 73us
       | OpGroupDecorate _ -> 74us
       | OpGroupMemberDecorate _ -> 75us
       | OpVectorExtractDynamic _ -> 77us
       | OpVectorInsertDynamic _ -> 78us
       | OpVectorShuffle _ -> 79us
       | OpCompositeConstruct _ -> 80us
       | OpCompositeExtract _ -> 81us
       | OpCompositeInsert _ -> 82us
       | OpCopyObject _ -> 83us
       | OpTranspose _ -> 84us
       | OpSampledImage _ -> 86us
       | OpImageSampleImplicitLod _ -> 87us
       | OpImageSampleExplicitLod _ -> 88us
       | OpImageSampleDrefImplicitLod _ -> 89us
       | OpImageSampleDrefExplicitLod _ -> 90us
       | OpImageSampleProjImplicitLod _ -> 91us
       | OpImageSampleProjExplicitLod _ -> 92us
       | OpImageSampleProjDrefImplicitLod _ -> 93us
       | OpImageSampleProjDrefExplicitLod _ -> 94us
       | OpImageFetch _ -> 95us
       | OpImageGather _ -> 96us
       | OpImageDrefGather _ -> 97us
       | OpImageRead _ -> 98us
       | OpImageWrite _ -> 99us
       | OpImage _ -> 100us
       | OpImageQueryFormat _ -> 101us
       | OpImageQueryOrder _ -> 102us
       | OpImageQuerySizeLod _ -> 103us
       | OpImageQuerySize _ -> 104us
       | OpImageQueryLod _ -> 105us
       | OpImageQueryLevels _ -> 106us
       | OpImageQuerySamples _ -> 107us
       | OpConvertFToU _ -> 109us
       | OpConvertFToS _ -> 110us
       | OpConvertSToF _ -> 111us
       | OpConvertUToF _ -> 112us
       | OpUConvert _ -> 113us
       | OpSConvert _ -> 114us
       | OpFConvert _ -> 115us
       | OpQuantizeToF16 _ -> 116us
       | OpConvertPtrToU _ -> 117us
       | OpSatConvertSToU _ -> 118us
       | OpSatConvertUToS _ -> 119us
       | OpConvertUToPtr _ -> 120us
       | OpPtrCastToGeneric _ -> 121us
       | OpGenericCastToPtr _ -> 122us
       | OpGenericCastToPtrExplicit _ -> 123us
       | OpBitcast _ -> 124us
       | OpSNegate _ -> 126us
       | OpFNegate _ -> 127us
       | OpIAdd _ -> 128us
       | OpFAdd _ -> 129us
       | OpISub _ -> 130us
       | OpFSub _ -> 131us
       | OpIMul _ -> 132us
       | OpFMul _ -> 133us
       | OpUDiv _ -> 134us
       | OpSDiv _ -> 135us
       | OpFDiv _ -> 136us
       | OpUMod _ -> 137us
       | OpSRem _ -> 138us
       | OpSMod _ -> 139us
       | OpFRem _ -> 140us
       | OpFMod _ -> 141us
       | OpVectorTimesScalar _ -> 142us
       | OpMatrixTimesScalar _ -> 143us
       | OpVectorTimesMatrix _ -> 144us
       | OpMatrixTimesVector _ -> 145us
       | OpMatrixTimesMatrix _ -> 146us
       | OpOuterProduct _ -> 147us
       | OpDot _ -> 148us
       | OpIAddCarry _ -> 149us
       | OpISubBorrow _ -> 150us
       | OpUMulExtended _ -> 151us
       | OpSMulExtended _ -> 152us
       | OpAny _ -> 154us
       | OpAll _ -> 155us
       | OpIsNan _ -> 156us
       | OpIsInf _ -> 157us
       | OpIsFinite _ -> 158us
       | OpIsNormal _ -> 159us
       | OpSignBitSet _ -> 160us
       | OpLessOrGreater _ -> 161us
       | OpOrdered _ -> 162us
       | OpUnordered _ -> 163us
       | OpLogicalEqual _ -> 164us
       | OpLogicalNotEqual _ -> 165us
       | OpLogicalOr _ -> 166us
       | OpLogicalAnd _ -> 167us
       | OpLogicalNot _ -> 168us
       | OpSelect _ -> 169us
       | OpIEqual _ -> 170us
       | OpINotEqual _ -> 171us
       | OpUGreaterThan _ -> 172us
       | OpSGreaterThan _ -> 173us
       | OpUGreaterThanEqual _ -> 174us
       | OpSGreaterThanEqual _ -> 175us
       | OpULessThan _ -> 176us
       | OpSLessThan _ -> 177us
       | OpULessThanEqual _ -> 178us
       | OpSLessThanEqual _ -> 179us
       | OpFOrdEqual _ -> 180us
       | OpFUnordEqual _ -> 181us
       | OpFOrdNotEqual _ -> 182us
       | OpFUnordNotEqual _ -> 183us
       | OpFOrdLessThan _ -> 184us
       | OpFUnordLessThan _ -> 185us
       | OpFOrdGreaterThan _ -> 186us
       | OpFUnordGreaterThan _ -> 187us
       | OpFOrdLessThanEqual _ -> 188us
       | OpFUnordLessThanEqual _ -> 189us
       | OpFOrdGreaterThanEqual _ -> 190us
       | OpFUnordGreaterThanEqual _ -> 191us
       | OpShiftRightLogical _ -> 194us
       | OpShiftRightArithmetic _ -> 195us
       | OpShiftLeftLogical _ -> 196us
       | OpBitwiseOr _ -> 197us
       | OpBitwiseXor _ -> 198us
       | OpBitwiseAnd _ -> 199us
       | OpNot _ -> 200us
       | OpBitFieldInsert _ -> 201us
       | OpBitFieldSExtract _ -> 202us
       | OpBitFieldUExtract _ -> 203us
       | OpBitReverse _ -> 204us
       | OpBitCount _ -> 205us
       | OpDPdx _ -> 207us
       | OpDPdy _ -> 208us
       | OpFwidth _ -> 209us
       | OpDPdxFine _ -> 210us
       | OpDPdyFine _ -> 211us
       | OpFwidthFine _ -> 212us
       | OpDPdxCoarse _ -> 213us
       | OpDPdyCoarse _ -> 214us
       | OpFwidthCoarse _ -> 215us
       | OpEmitVertex -> 218us
       | OpEndPrimitive -> 219us
       | OpEmitStreamVertex _ -> 220us
       | OpEndStreamPrimitive _ -> 221us
       | OpControlBarrier _ -> 224us
       | OpMemoryBarrier _ -> 225us
       | OpAtomicLoad _ -> 227us
       | OpAtomicStore _ -> 228us
       | OpAtomicExchange _ -> 229us
       | OpAtomicCompareExchange _ -> 230us
       | OpAtomicCompareExchangeWeak _ -> 231us
       | OpAtomicIIncrement _ -> 232us
       | OpAtomicIDecrement _ -> 233us
       | OpAtomicIAdd _ -> 234us
       | OpAtomicISub _ -> 235us
       | OpAtomicSMin _ -> 236us
       | OpAtomicUMin _ -> 237us
       | OpAtomicSMax _ -> 238us
       | OpAtomicUMax _ -> 239us
       | OpAtomicAnd _ -> 240us
       | OpAtomicOr _ -> 241us
       | OpAtomicXor _ -> 242us
       | OpPhi _ -> 245us
       | OpLoopMerge _ -> 246us
       | OpSelectionMerge _ -> 247us
       | OpLabel _ -> 248us
       | OpBranch _ -> 249us
       | OpBranchConditional _ -> 250us
       | OpSwitch _ -> 251us
       | OpKill -> 252us
       | OpReturn -> 253us
       | OpReturnValue _ -> 254us
       | OpUnreachable -> 255us
       | OpLifetimeStart _ -> 256us
       | OpLifetimeStop _ -> 257us
       | OpGroupAsyncCopy _ -> 259us
       | OpGroupWaitEvents _ -> 260us
       | OpGroupAll _ -> 261us
       | OpGroupAny _ -> 262us
       | OpGroupBroadcast _ -> 263us
       | OpGroupIAdd _ -> 264us
       | OpGroupFAdd _ -> 265us
       | OpGroupFMin _ -> 266us
       | OpGroupUMin _ -> 267us
       | OpGroupSMin _ -> 268us
       | OpGroupFMax _ -> 269us
       | OpGroupUMax _ -> 270us
       | OpGroupSMax _ -> 271us
       | OpReadPipe _ -> 274us
       | OpWritePipe _ -> 275us
       | OpReservedReadPipe _ -> 276us
       | OpReservedWritePipe _ -> 277us
       | OpReserveReadPipePackets _ -> 278us
       | OpReserveWritePipePackets _ -> 279us
       | OpCommitReadPipe _ -> 280us
       | OpCommitWritePipe _ -> 281us
       | OpIsValidReserveId _ -> 282us
       | OpGetNumPipePackets _ -> 283us
       | OpGetMaxPipePackets _ -> 284us
       | OpGroupReserveReadPipePackets _ -> 285us
       | OpGroupReserveWritePipePackets _ -> 286us
       | OpGroupCommitReadPipe _ -> 287us
       | OpGroupCommitWritePipe _ -> 288us
       | OpEnqueueMarker _ -> 291us
       | OpEnqueueKernel _ -> 292us
       | OpGetKernelNDrangeSubGroupCount _ -> 293us
       | OpGetKernelNDrangeMaxSubGroupSize _ -> 294us
       | OpGetKernelWorkGroupSize _ -> 295us
       | OpGetKernelPreferredWorkGroupSizeMultiple _ -> 296us
       | OpRetainEvent _ -> 297us
       | OpReleaseEvent _ -> 298us
       | OpCreateUserEvent _ -> 299us
       | OpIsValidEvent _ -> 300us
       | OpSetUserEventStatus _ -> 301us
       | OpCaptureEventProfilingInfo _ -> 302us
       | OpGetDefaultQueue _ -> 303us
       | OpBuildNDRange _ -> 304us
       | OpImageSparseSampleImplicitLod _ -> 305us
       | OpImageSparseSampleExplicitLod _ -> 306us
       | OpImageSparseSampleDrefImplicitLod _ -> 307us
       | OpImageSparseSampleDrefExplicitLod _ -> 308us
       | OpImageSparseSampleProjImplicitLod _ -> 309us
       | OpImageSparseSampleProjExplicitLod _ -> 310us
       | OpImageSparseSampleProjDrefImplicitLod _ -> 311us
       | OpImageSparseSampleProjDrefExplicitLod _ -> 312us
       | OpImageSparseFetch _ -> 313us
       | OpImageSparseGather _ -> 314us
       | OpImageSparseDrefGather _ -> 315us
       | OpImageSparseTexelsResident _ -> 316us
       | OpNoLine -> 317us
       | OpAtomicFlagTestAndSet _ -> 318us
       | OpAtomicFlagClear _ -> 319us
       | OpImageSparseRead _ -> 320us
       | OpSizeOf _ -> 321us
       | OpTypePipeStorage _ -> 322us
       | OpConstantPipeStorage _ -> 323us
       | OpCreatePipeFromPipeStorage _ -> 324us
       | OpGetKernelLocalSizeForSubgroupCount _ -> 325us
       | OpGetKernelMaxNumSubgroups _ -> 326us
       | OpTypeNamedBarrier _ -> 327us
       | OpNamedBarrierInitialize _ -> 328us
       | OpMemoryNamedBarrier _ -> 329us
       | OpModuleProcessed _ -> 330us
       | OpExecutionModeId _ -> 331us
       | OpDecorateId _ -> 332us
       | OpGroupNonUniformElect _ -> 333us
       | OpGroupNonUniformAll _ -> 334us
       | OpGroupNonUniformAny _ -> 335us
       | OpGroupNonUniformAllEqual _ -> 336us
       | OpGroupNonUniformBroadcast _ -> 337us
       | OpGroupNonUniformBroadcastFirst _ -> 338us
       | OpGroupNonUniformBallot _ -> 339us
       | OpGroupNonUniformInverseBallot _ -> 340us
       | OpGroupNonUniformBallotBitExtract _ -> 341us
       | OpGroupNonUniformBallotBitCount _ -> 342us
       | OpGroupNonUniformBallotFindLSB _ -> 343us
       | OpGroupNonUniformBallotFindMSB _ -> 344us
       | OpGroupNonUniformShuffle _ -> 345us
       | OpGroupNonUniformShuffleXor _ -> 346us
       | OpGroupNonUniformShuffleUp _ -> 347us
       | OpGroupNonUniformShuffleDown _ -> 348us
       | OpGroupNonUniformIAdd _ -> 349us
       | OpGroupNonUniformFAdd _ -> 350us
       | OpGroupNonUniformIMul _ -> 351us
       | OpGroupNonUniformFMul _ -> 352us
       | OpGroupNonUniformSMin _ -> 353us
       | OpGroupNonUniformUMin _ -> 354us
       | OpGroupNonUniformFMin _ -> 355us
       | OpGroupNonUniformSMax _ -> 356us
       | OpGroupNonUniformUMax _ -> 357us
       | OpGroupNonUniformFMax _ -> 358us
       | OpGroupNonUniformBitwiseAnd _ -> 359us
       | OpGroupNonUniformBitwiseOr _ -> 360us
       | OpGroupNonUniformBitwiseXor _ -> 361us
       | OpGroupNonUniformLogicalAnd _ -> 362us
       | OpGroupNonUniformLogicalOr _ -> 363us
       | OpGroupNonUniformLogicalXor _ -> 364us
       | OpGroupNonUniformQuadBroadcast _ -> 365us
       | OpGroupNonUniformQuadSwap _ -> 366us
       | OpCopyLogical _ -> 400us
       | OpPtrEqual _ -> 401us
       | OpPtrNotEqual _ -> 402us
       | OpPtrDiff _ -> 403us
       | OpColorAttachmentReadEXT _ -> 4160us
       | OpDepthAttachmentReadEXT _ -> 4161us
       | OpStencilAttachmentReadEXT _ -> 4162us
       | OpTerminateInvocation -> 4416us
       | OpTypeUntypedPointerKHR _ -> 4417us
       | OpUntypedVariableKHR _ -> 4418us
       | OpUntypedAccessChainKHR _ -> 4419us
       | OpUntypedInBoundsAccessChainKHR _ -> 4420us
       | OpSubgroupBallotKHR _ -> 4421us
       | OpSubgroupFirstInvocationKHR _ -> 4422us
       | OpUntypedPtrAccessChainKHR _ -> 4423us
       | OpUntypedInBoundsPtrAccessChainKHR _ -> 4424us
       | OpUntypedArrayLengthKHR _ -> 4425us
       | OpUntypedPrefetchKHR _ -> 4426us
       | OpSubgroupAllKHR _ -> 4428us
       | OpSubgroupAnyKHR _ -> 4429us
       | OpSubgroupAllEqualKHR _ -> 4430us
       | OpGroupNonUniformRotateKHR _ -> 4431us
       | OpSubgroupReadInvocationKHR _ -> 4432us
       | OpExtInstWithForwardRefsKHR _ -> 4433us
       | OpTraceRayKHR _ -> 4445us
       | OpExecuteCallableKHR _ -> 4446us
       | OpConvertUToAccelerationStructureKHR _ -> 4447us
       | OpIgnoreIntersectionKHR -> 4448us
       | OpTerminateRayKHR -> 4449us
       | OpSDot _ -> 4450us
       | OpUDot _ -> 4451us
       | OpSUDot _ -> 4452us
       | OpSDotAccSat _ -> 4453us
       | OpUDotAccSat _ -> 4454us
       | OpSUDotAccSat _ -> 4455us
       | OpTypeCooperativeMatrixKHR _ -> 4456us
       | OpCooperativeMatrixLoadKHR _ -> 4457us
       | OpCooperativeMatrixStoreKHR _ -> 4458us
       | OpCooperativeMatrixMulAddKHR _ -> 4459us
       | OpCooperativeMatrixLengthKHR _ -> 4460us
       | OpConstantCompositeReplicateEXT _ -> 4461us
       | OpSpecConstantCompositeReplicateEXT _ -> 4462us
       | OpCompositeConstructReplicateEXT _ -> 4463us
       | OpTypeRayQueryKHR _ -> 4472us
       | OpRayQueryInitializeKHR _ -> 4473us
       | OpRayQueryTerminateKHR _ -> 4474us
       | OpRayQueryGenerateIntersectionKHR _ -> 4475us
       | OpRayQueryConfirmIntersectionKHR _ -> 4476us
       | OpRayQueryProceedKHR _ -> 4477us
       | OpRayQueryGetIntersectionTypeKHR _ -> 4479us
       | OpImageSampleWeightedQCOM _ -> 4480us
       | OpImageBoxFilterQCOM _ -> 4481us
       | OpImageBlockMatchSSDQCOM _ -> 4482us
       | OpImageBlockMatchSADQCOM _ -> 4483us
       | OpImageBlockMatchWindowSSDQCOM _ -> 4500us
       | OpImageBlockMatchWindowSADQCOM _ -> 4501us
       | OpImageBlockMatchGatherSSDQCOM _ -> 4502us
       | OpImageBlockMatchGatherSADQCOM _ -> 4503us
       | OpGroupIAddNonUniformAMD _ -> 5000us
       | OpGroupFAddNonUniformAMD _ -> 5001us
       | OpGroupFMinNonUniformAMD _ -> 5002us
       | OpGroupUMinNonUniformAMD _ -> 5003us
       | OpGroupSMinNonUniformAMD _ -> 5004us
       | OpGroupFMaxNonUniformAMD _ -> 5005us
       | OpGroupUMaxNonUniformAMD _ -> 5006us
       | OpGroupSMaxNonUniformAMD _ -> 5007us
       | OpFragmentMaskFetchAMD _ -> 5011us
       | OpFragmentFetchAMD _ -> 5012us
       | OpReadClockKHR _ -> 5056us
       | OpAllocateNodePayloadsAMDX _ -> 5074us
       | OpEnqueueNodePayloadsAMDX _ -> 5075us
       | OpTypeNodePayloadArrayAMDX _ -> 5076us
       | OpFinishWritingNodePayloadAMDX _ -> 5078us
       | OpNodePayloadArrayLengthAMDX _ -> 5090us
       | OpIsNodePayloadValidAMDX _ -> 5101us
       | OpConstantStringAMDX _ -> 5103us
       | OpSpecConstantStringAMDX _ -> 5104us
       | OpGroupNonUniformQuadAllKHR _ -> 5110us
       | OpGroupNonUniformQuadAnyKHR _ -> 5111us
       | OpHitObjectRecordHitMotionNV _ -> 5249us
       | OpHitObjectRecordHitWithIndexMotionNV _ -> 5250us
       | OpHitObjectRecordMissMotionNV _ -> 5251us
       | OpHitObjectGetWorldToObjectNV _ -> 5252us
       | OpHitObjectGetObjectToWorldNV _ -> 5253us
       | OpHitObjectGetObjectRayDirectionNV _ -> 5254us
       | OpHitObjectGetObjectRayOriginNV _ -> 5255us
       | OpHitObjectTraceRayMotionNV _ -> 5256us
       | OpHitObjectGetShaderRecordBufferHandleNV _ -> 5257us
       | OpHitObjectGetShaderBindingTableRecordIndexNV _ -> 5258us
       | OpHitObjectRecordEmptyNV _ -> 5259us
       | OpHitObjectTraceRayNV _ -> 5260us
       | OpHitObjectRecordHitNV _ -> 5261us
       | OpHitObjectRecordHitWithIndexNV _ -> 5262us
       | OpHitObjectRecordMissNV _ -> 5263us
       | OpHitObjectExecuteShaderNV _ -> 5264us
       | OpHitObjectGetCurrentTimeNV _ -> 5265us
       | OpHitObjectGetAttributesNV _ -> 5266us
       | OpHitObjectGetHitKindNV _ -> 5267us
       | OpHitObjectGetPrimitiveIndexNV _ -> 5268us
       | OpHitObjectGetGeometryIndexNV _ -> 5269us
       | OpHitObjectGetInstanceIdNV _ -> 5270us
       | OpHitObjectGetInstanceCustomIndexNV _ -> 5271us
       | OpHitObjectGetWorldRayDirectionNV _ -> 5272us
       | OpHitObjectGetWorldRayOriginNV _ -> 5273us
       | OpHitObjectGetRayTMaxNV _ -> 5274us
       | OpHitObjectGetRayTMinNV _ -> 5275us
       | OpHitObjectIsEmptyNV _ -> 5276us
       | OpHitObjectIsHitNV _ -> 5277us
       | OpHitObjectIsMissNV _ -> 5278us
       | OpReorderThreadWithHitObjectNV _ -> 5279us
       | OpReorderThreadWithHintNV _ -> 5280us
       | OpTypeHitObjectNV _ -> 5281us
       | OpImageSampleFootprintNV _ -> 5283us
       | OpCooperativeMatrixConvertNV _ -> 5293us
       | OpEmitMeshTasksEXT _ -> 5294us
       | OpSetMeshOutputsEXT _ -> 5295us
       | OpGroupNonUniformPartitionNV _ -> 5296us
       | OpWritePackedPrimitiveIndices4x8NV _ -> 5299us
       | OpFetchMicroTriangleVertexPositionNV _ -> 5300us
       | OpFetchMicroTriangleVertexBarycentricNV _ -> 5301us
       | OpReportIntersectionKHR _ -> 5334us
       | OpIgnoreIntersectionNV -> 5335us
       | OpTerminateRayNV -> 5336us
       | OpTraceNV _ -> 5337us
       | OpTraceMotionNV _ -> 5338us
       | OpTraceRayMotionNV _ -> 5339us
       | OpRayQueryGetIntersectionTriangleVertexPositionsKHR _ -> 5340us
       | OpTypeAccelerationStructureKHR _ -> 5341us
       | OpExecuteCallableNV _ -> 5344us
       | OpTypeCooperativeMatrixNV _ -> 5358us
       | OpCooperativeMatrixLoadNV _ -> 5359us
       | OpCooperativeMatrixStoreNV _ -> 5360us
       | OpCooperativeMatrixMulAddNV _ -> 5361us
       | OpCooperativeMatrixLengthNV _ -> 5362us
       | OpBeginInvocationInterlockEXT -> 5364us
       | OpEndInvocationInterlockEXT -> 5365us
       | OpCooperativeMatrixReduceNV _ -> 5366us
       | OpCooperativeMatrixLoadTensorNV _ -> 5367us
       | OpCooperativeMatrixStoreTensorNV _ -> 5368us
       | OpCooperativeMatrixPerElementOpNV _ -> 5369us
       | OpTypeTensorLayoutNV _ -> 5370us
       | OpTypeTensorViewNV _ -> 5371us
       | OpCreateTensorLayoutNV _ -> 5372us
       | OpTensorLayoutSetDimensionNV _ -> 5373us
       | OpTensorLayoutSetStrideNV _ -> 5374us
       | OpTensorLayoutSliceNV _ -> 5375us
       | OpTensorLayoutSetClampValueNV _ -> 5376us
       | OpCreateTensorViewNV _ -> 5377us
       | OpTensorViewSetDimensionNV _ -> 5378us
       | OpTensorViewSetStrideNV _ -> 5379us
       | OpDemoteToHelperInvocation -> 5380us
       | OpIsHelperInvocationEXT _ -> 5381us
       | OpTensorViewSetClipNV _ -> 5382us
       | OpTensorLayoutSetBlockSizeNV _ -> 5384us
       | OpCooperativeMatrixTransposeNV _ -> 5390us
       | OpConvertUToImageNV _ -> 5391us
       | OpConvertUToSamplerNV _ -> 5392us
       | OpConvertImageToUNV _ -> 5393us
       | OpConvertSamplerToUNV _ -> 5394us
       | OpConvertUToSampledImageNV _ -> 5395us
       | OpConvertSampledImageToUNV _ -> 5396us
       | OpSamplerImageAddressingModeNV _ -> 5397us
       | OpRawAccessChainNV _ -> 5398us
       | OpSubgroupShuffleINTEL _ -> 5571us
       | OpSubgroupShuffleDownINTEL _ -> 5572us
       | OpSubgroupShuffleUpINTEL _ -> 5573us
       | OpSubgroupShuffleXorINTEL _ -> 5574us
       | OpSubgroupBlockReadINTEL _ -> 5575us
       | OpSubgroupBlockWriteINTEL _ -> 5576us
       | OpSubgroupImageBlockReadINTEL _ -> 5577us
       | OpSubgroupImageBlockWriteINTEL _ -> 5578us
       | OpSubgroupImageMediaBlockReadINTEL _ -> 5580us
       | OpSubgroupImageMediaBlockWriteINTEL _ -> 5581us
       | OpUCountLeadingZerosINTEL _ -> 5585us
       | OpUCountTrailingZerosINTEL _ -> 5586us
       | OpAbsISubINTEL _ -> 5587us
       | OpAbsUSubINTEL _ -> 5588us
       | OpIAddSatINTEL _ -> 5589us
       | OpUAddSatINTEL _ -> 5590us
       | OpIAverageINTEL _ -> 5591us
       | OpUAverageINTEL _ -> 5592us
       | OpIAverageRoundedINTEL _ -> 5593us
       | OpUAverageRoundedINTEL _ -> 5594us
       | OpISubSatINTEL _ -> 5595us
       | OpUSubSatINTEL _ -> 5596us
       | OpIMul32x16INTEL _ -> 5597us
       | OpUMul32x16INTEL _ -> 5598us
       | OpConstantFunctionPointerINTEL _ -> 5600us
       | OpFunctionPointerCallINTEL _ -> 5601us
       | OpAsmTargetINTEL _ -> 5609us
       | OpAsmINTEL _ -> 5610us
       | OpAsmCallINTEL _ -> 5611us
       | OpAtomicFMinEXT _ -> 5614us
       | OpAtomicFMaxEXT _ -> 5615us
       | OpAssumeTrueKHR _ -> 5630us
       | OpExpectKHR _ -> 5631us
       | OpDecorateString _ -> 5632us
       | OpMemberDecorateString _ -> 5633us
       | OpVmeImageINTEL _ -> 5699us
       | OpTypeVmeImageINTEL _ -> 5700us
       | OpTypeAvcImePayloadINTEL _ -> 5701us
       | OpTypeAvcRefPayloadINTEL _ -> 5702us
       | OpTypeAvcSicPayloadINTEL _ -> 5703us
       | OpTypeAvcMcePayloadINTEL _ -> 5704us
       | OpTypeAvcMceResultINTEL _ -> 5705us
       | OpTypeAvcImeResultINTEL _ -> 5706us
       | OpTypeAvcImeResultSingleReferenceStreamoutINTEL _ -> 5707us
       | OpTypeAvcImeResultDualReferenceStreamoutINTEL _ -> 5708us
       | OpTypeAvcImeSingleReferenceStreaminINTEL _ -> 5709us
       | OpTypeAvcImeDualReferenceStreaminINTEL _ -> 5710us
       | OpTypeAvcRefResultINTEL _ -> 5711us
       | OpTypeAvcSicResultINTEL _ -> 5712us
       | OpSubgroupAvcMceGetDefaultInterBaseMultiReferencePenaltyINTEL _ -> 5713us
       | OpSubgroupAvcMceSetInterBaseMultiReferencePenaltyINTEL _ -> 5714us
       | OpSubgroupAvcMceGetDefaultInterShapePenaltyINTEL _ -> 5715us
       | OpSubgroupAvcMceSetInterShapePenaltyINTEL _ -> 5716us
       | OpSubgroupAvcMceGetDefaultInterDirectionPenaltyINTEL _ -> 5717us
       | OpSubgroupAvcMceSetInterDirectionPenaltyINTEL _ -> 5718us
       | OpSubgroupAvcMceGetDefaultIntraLumaShapePenaltyINTEL _ -> 5719us
       | OpSubgroupAvcMceGetDefaultInterMotionVectorCostTableINTEL _ -> 5720us
       | OpSubgroupAvcMceGetDefaultHighPenaltyCostTableINTEL _ -> 5721us
       | OpSubgroupAvcMceGetDefaultMediumPenaltyCostTableINTEL _ -> 5722us
       | OpSubgroupAvcMceGetDefaultLowPenaltyCostTableINTEL _ -> 5723us
       | OpSubgroupAvcMceSetMotionVectorCostFunctionINTEL _ -> 5724us
       | OpSubgroupAvcMceGetDefaultIntraLumaModePenaltyINTEL _ -> 5725us
       | OpSubgroupAvcMceGetDefaultNonDcLumaIntraPenaltyINTEL _ -> 5726us
       | OpSubgroupAvcMceGetDefaultIntraChromaModeBasePenaltyINTEL _ -> 5727us
       | OpSubgroupAvcMceSetAcOnlyHaarINTEL _ -> 5728us
       | OpSubgroupAvcMceSetSourceInterlacedFieldPolarityINTEL _ -> 5729us
       | OpSubgroupAvcMceSetSingleReferenceInterlacedFieldPolarityINTEL _ -> 5730us
       | OpSubgroupAvcMceSetDualReferenceInterlacedFieldPolaritiesINTEL _ -> 5731us
       | OpSubgroupAvcMceConvertToImePayloadINTEL _ -> 5732us
       | OpSubgroupAvcMceConvertToImeResultINTEL _ -> 5733us
       | OpSubgroupAvcMceConvertToRefPayloadINTEL _ -> 5734us
       | OpSubgroupAvcMceConvertToRefResultINTEL _ -> 5735us
       | OpSubgroupAvcMceConvertToSicPayloadINTEL _ -> 5736us
       | OpSubgroupAvcMceConvertToSicResultINTEL _ -> 5737us
       | OpSubgroupAvcMceGetMotionVectorsINTEL _ -> 5738us
       | OpSubgroupAvcMceGetInterDistortionsINTEL _ -> 5739us
       | OpSubgroupAvcMceGetBestInterDistortionsINTEL _ -> 5740us
       | OpSubgroupAvcMceGetInterMajorShapeINTEL _ -> 5741us
       | OpSubgroupAvcMceGetInterMinorShapeINTEL _ -> 5742us
       | OpSubgroupAvcMceGetInterDirectionsINTEL _ -> 5743us
       | OpSubgroupAvcMceGetInterMotionVectorCountINTEL _ -> 5744us
       | OpSubgroupAvcMceGetInterReferenceIdsINTEL _ -> 5745us
       | OpSubgroupAvcMceGetInterReferenceInterlacedFieldPolaritiesINTEL _ -> 5746us
       | OpSubgroupAvcImeInitializeINTEL _ -> 5747us
       | OpSubgroupAvcImeSetSingleReferenceINTEL _ -> 5748us
       | OpSubgroupAvcImeSetDualReferenceINTEL _ -> 5749us
       | OpSubgroupAvcImeRefWindowSizeINTEL _ -> 5750us
       | OpSubgroupAvcImeAdjustRefOffsetINTEL _ -> 5751us
       | OpSubgroupAvcImeConvertToMcePayloadINTEL _ -> 5752us
       | OpSubgroupAvcImeSetMaxMotionVectorCountINTEL _ -> 5753us
       | OpSubgroupAvcImeSetUnidirectionalMixDisableINTEL _ -> 5754us
       | OpSubgroupAvcImeSetEarlySearchTerminationThresholdINTEL _ -> 5755us
       | OpSubgroupAvcImeSetWeightedSadINTEL _ -> 5756us
       | OpSubgroupAvcImeEvaluateWithSingleReferenceINTEL _ -> 5757us
       | OpSubgroupAvcImeEvaluateWithDualReferenceINTEL _ -> 5758us
       | OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminINTEL _ -> 5759us
       | OpSubgroupAvcImeEvaluateWithDualReferenceStreaminINTEL _ -> 5760us
       | OpSubgroupAvcImeEvaluateWithSingleReferenceStreamoutINTEL _ -> 5761us
       | OpSubgroupAvcImeEvaluateWithDualReferenceStreamoutINTEL _ -> 5762us
       | OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminoutINTEL _ -> 5763us
       | OpSubgroupAvcImeEvaluateWithDualReferenceStreaminoutINTEL _ -> 5764us
       | OpSubgroupAvcImeConvertToMceResultINTEL _ -> 5765us
       | OpSubgroupAvcImeGetSingleReferenceStreaminINTEL _ -> 5766us
       | OpSubgroupAvcImeGetDualReferenceStreaminINTEL _ -> 5767us
       | OpSubgroupAvcImeStripSingleReferenceStreamoutINTEL _ -> 5768us
       | OpSubgroupAvcImeStripDualReferenceStreamoutINTEL _ -> 5769us
       | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeMotionVectorsINTEL _ -> 5770us
       | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeDistortionsINTEL _ -> 5771us
       | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeReferenceIdsINTEL _ -> 5772us
       | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeMotionVectorsINTEL _ -> 5773us
       | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeDistortionsINTEL _ -> 5774us
       | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeReferenceIdsINTEL _ -> 5775us
       | OpSubgroupAvcImeGetBorderReachedINTEL _ -> 5776us
       | OpSubgroupAvcImeGetTruncatedSearchIndicationINTEL _ -> 5777us
       | OpSubgroupAvcImeGetUnidirectionalEarlySearchTerminationINTEL _ -> 5778us
       | OpSubgroupAvcImeGetWeightingPatternMinimumMotionVectorINTEL _ -> 5779us
       | OpSubgroupAvcImeGetWeightingPatternMinimumDistortionINTEL _ -> 5780us
       | OpSubgroupAvcFmeInitializeINTEL _ -> 5781us
       | OpSubgroupAvcBmeInitializeINTEL _ -> 5782us
       | OpSubgroupAvcRefConvertToMcePayloadINTEL _ -> 5783us
       | OpSubgroupAvcRefSetBidirectionalMixDisableINTEL _ -> 5784us
       | OpSubgroupAvcRefSetBilinearFilterEnableINTEL _ -> 5785us
       | OpSubgroupAvcRefEvaluateWithSingleReferenceINTEL _ -> 5786us
       | OpSubgroupAvcRefEvaluateWithDualReferenceINTEL _ -> 5787us
       | OpSubgroupAvcRefEvaluateWithMultiReferenceINTEL _ -> 5788us
       | OpSubgroupAvcRefEvaluateWithMultiReferenceInterlacedINTEL _ -> 5789us
       | OpSubgroupAvcRefConvertToMceResultINTEL _ -> 5790us
       | OpSubgroupAvcSicInitializeINTEL _ -> 5791us
       | OpSubgroupAvcSicConfigureSkcINTEL _ -> 5792us
       | OpSubgroupAvcSicConfigureIpeLumaINTEL _ -> 5793us
       | OpSubgroupAvcSicConfigureIpeLumaChromaINTEL _ -> 5794us
       | OpSubgroupAvcSicGetMotionVectorMaskINTEL _ -> 5795us
       | OpSubgroupAvcSicConvertToMcePayloadINTEL _ -> 5796us
       | OpSubgroupAvcSicSetIntraLumaShapePenaltyINTEL _ -> 5797us
       | OpSubgroupAvcSicSetIntraLumaModeCostFunctionINTEL _ -> 5798us
       | OpSubgroupAvcSicSetIntraChromaModeCostFunctionINTEL _ -> 5799us
       | OpSubgroupAvcSicSetBilinearFilterEnableINTEL _ -> 5800us
       | OpSubgroupAvcSicSetSkcForwardTransformEnableINTEL _ -> 5801us
       | OpSubgroupAvcSicSetBlockBasedRawSkipSadINTEL _ -> 5802us
       | OpSubgroupAvcSicEvaluateIpeINTEL _ -> 5803us
       | OpSubgroupAvcSicEvaluateWithSingleReferenceINTEL _ -> 5804us
       | OpSubgroupAvcSicEvaluateWithDualReferenceINTEL _ -> 5805us
       | OpSubgroupAvcSicEvaluateWithMultiReferenceINTEL _ -> 5806us
       | OpSubgroupAvcSicEvaluateWithMultiReferenceInterlacedINTEL _ -> 5807us
       | OpSubgroupAvcSicConvertToMceResultINTEL _ -> 5808us
       | OpSubgroupAvcSicGetIpeLumaShapeINTEL _ -> 5809us
       | OpSubgroupAvcSicGetBestIpeLumaDistortionINTEL _ -> 5810us
       | OpSubgroupAvcSicGetBestIpeChromaDistortionINTEL _ -> 5811us
       | OpSubgroupAvcSicGetPackedIpeLumaModesINTEL _ -> 5812us
       | OpSubgroupAvcSicGetIpeChromaModeINTEL _ -> 5813us
       | OpSubgroupAvcSicGetPackedSkcLumaCountThresholdINTEL _ -> 5814us
       | OpSubgroupAvcSicGetPackedSkcLumaSumThresholdINTEL _ -> 5815us
       | OpSubgroupAvcSicGetInterRawSadsINTEL _ -> 5816us
       | OpVariableLengthArrayINTEL _ -> 5818us
       | OpSaveMemoryINTEL _ -> 5819us
       | OpRestoreMemoryINTEL _ -> 5820us
       | OpArbitraryFloatSinCosPiINTEL _ -> 5840us
       | OpArbitraryFloatCastINTEL _ -> 5841us
       | OpArbitraryFloatCastFromIntINTEL _ -> 5842us
       | OpArbitraryFloatCastToIntINTEL _ -> 5843us
       | OpArbitraryFloatAddINTEL _ -> 5846us
       | OpArbitraryFloatSubINTEL _ -> 5847us
       | OpArbitraryFloatMulINTEL _ -> 5848us
       | OpArbitraryFloatDivINTEL _ -> 5849us
       | OpArbitraryFloatGTINTEL _ -> 5850us
       | OpArbitraryFloatGEINTEL _ -> 5851us
       | OpArbitraryFloatLTINTEL _ -> 5852us
       | OpArbitraryFloatLEINTEL _ -> 5853us
       | OpArbitraryFloatEQINTEL _ -> 5854us
       | OpArbitraryFloatRecipINTEL _ -> 5855us
       | OpArbitraryFloatRSqrtINTEL _ -> 5856us
       | OpArbitraryFloatCbrtINTEL _ -> 5857us
       | OpArbitraryFloatHypotINTEL _ -> 5858us
       | OpArbitraryFloatSqrtINTEL _ -> 5859us
       | OpArbitraryFloatLogINTEL _ -> 5860us
       | OpArbitraryFloatLog2INTEL _ -> 5861us
       | OpArbitraryFloatLog10INTEL _ -> 5862us
       | OpArbitraryFloatLog1pINTEL _ -> 5863us
       | OpArbitraryFloatExpINTEL _ -> 5864us
       | OpArbitraryFloatExp2INTEL _ -> 5865us
       | OpArbitraryFloatExp10INTEL _ -> 5866us
       | OpArbitraryFloatExpm1INTEL _ -> 5867us
       | OpArbitraryFloatSinINTEL _ -> 5868us
       | OpArbitraryFloatCosINTEL _ -> 5869us
       | OpArbitraryFloatSinCosINTEL _ -> 5870us
       | OpArbitraryFloatSinPiINTEL _ -> 5871us
       | OpArbitraryFloatCosPiINTEL _ -> 5872us
       | OpArbitraryFloatASinINTEL _ -> 5873us
       | OpArbitraryFloatASinPiINTEL _ -> 5874us
       | OpArbitraryFloatACosINTEL _ -> 5875us
       | OpArbitraryFloatACosPiINTEL _ -> 5876us
       | OpArbitraryFloatATanINTEL _ -> 5877us
       | OpArbitraryFloatATanPiINTEL _ -> 5878us
       | OpArbitraryFloatATan2INTEL _ -> 5879us
       | OpArbitraryFloatPowINTEL _ -> 5880us
       | OpArbitraryFloatPowRINTEL _ -> 5881us
       | OpArbitraryFloatPowNINTEL _ -> 5882us
       | OpLoopControlINTEL _ -> 5887us
       | OpAliasDomainDeclINTEL _ -> 5911us
       | OpAliasScopeDeclINTEL _ -> 5912us
       | OpAliasScopeListDeclINTEL _ -> 5913us
       | OpFixedSqrtINTEL _ -> 5923us
       | OpFixedRecipINTEL _ -> 5924us
       | OpFixedRsqrtINTEL _ -> 5925us
       | OpFixedSinINTEL _ -> 5926us
       | OpFixedCosINTEL _ -> 5927us
       | OpFixedSinCosINTEL _ -> 5928us
       | OpFixedSinPiINTEL _ -> 5929us
       | OpFixedCosPiINTEL _ -> 5930us
       | OpFixedSinCosPiINTEL _ -> 5931us
       | OpFixedLogINTEL _ -> 5932us
       | OpFixedExpINTEL _ -> 5933us
       | OpPtrCastToCrossWorkgroupINTEL _ -> 5934us
       | OpCrossWorkgroupCastToPtrINTEL _ -> 5938us
       | OpReadPipeBlockingINTEL _ -> 5946us
       | OpWritePipeBlockingINTEL _ -> 5947us
       | OpFPGARegINTEL _ -> 5949us
       | OpRayQueryGetRayTMinKHR _ -> 6016us
       | OpRayQueryGetRayFlagsKHR _ -> 6017us
       | OpRayQueryGetIntersectionTKHR _ -> 6018us
       | OpRayQueryGetIntersectionInstanceCustomIndexKHR _ -> 6019us
       | OpRayQueryGetIntersectionInstanceIdKHR _ -> 6020us
       | OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR _ -> 6021us
       | OpRayQueryGetIntersectionGeometryIndexKHR _ -> 6022us
       | OpRayQueryGetIntersectionPrimitiveIndexKHR _ -> 6023us
       | OpRayQueryGetIntersectionBarycentricsKHR _ -> 6024us
       | OpRayQueryGetIntersectionFrontFaceKHR _ -> 6025us
       | OpRayQueryGetIntersectionCandidateAABBOpaqueKHR _ -> 6026us
       | OpRayQueryGetIntersectionObjectRayDirectionKHR _ -> 6027us
       | OpRayQueryGetIntersectionObjectRayOriginKHR _ -> 6028us
       | OpRayQueryGetWorldRayDirectionKHR _ -> 6029us
       | OpRayQueryGetWorldRayOriginKHR _ -> 6030us
       | OpRayQueryGetIntersectionObjectToWorldKHR _ -> 6031us
       | OpRayQueryGetIntersectionWorldToObjectKHR _ -> 6032us
       | OpAtomicFAddEXT _ -> 6035us
       | OpTypeBufferSurfaceINTEL _ -> 6086us
       | OpTypeStructContinuedINTEL _ -> 6090us
       | OpConstantCompositeContinuedINTEL _ -> 6091us
       | OpSpecConstantCompositeContinuedINTEL _ -> 6092us
       | OpCompositeConstructContinuedINTEL _ -> 6096us
       | OpConvertFToBF16INTEL _ -> 6116us
       | OpConvertBF16ToFINTEL _ -> 6117us
       | OpControlBarrierArriveINTEL _ -> 6142us
       | OpControlBarrierWaitINTEL _ -> 6143us
       | OpArithmeticFenceEXT _ -> 6145us
       | OpSubgroupBlockPrefetchINTEL _ -> 6221us
       | OpGroupIMulKHR _ -> 6401us
       | OpGroupFMulKHR _ -> 6402us
       | OpGroupBitwiseAndKHR _ -> 6403us
       | OpGroupBitwiseOrKHR _ -> 6404us
       | OpGroupBitwiseXorKHR _ -> 6405us
       | OpGroupLogicalAndKHR _ -> 6406us
       | OpGroupLogicalOrKHR _ -> 6407us
       | OpGroupLogicalXorKHR _ -> 6408us
       | OpMaskedGatherINTEL _ -> 6428us
       | OpMaskedScatterINTEL _ -> 6429us

    member x.Version =
       match x with
       | OpNop -> 65536u
       | OpUndef _ -> 65536u
       | OpSourceContinued _ -> 65536u
       | OpSource _ -> 65536u
       | OpSourceExtension _ -> 65536u
       | OpName _ -> 65536u
       | OpMemberName _ -> 65536u
       | OpString _ -> 65536u
       | OpLine _ -> 65536u
       | OpExtension _ -> 65536u
       | OpExtInstImport _ -> 65536u
       | OpExtInst _ -> 65536u
       | OpMemoryModel _ -> 65536u
       | OpEntryPoint _ -> 65536u
       | OpExecutionMode _ -> 65536u
       | OpCapability _ -> 65536u
       | OpTypeVoid _ -> 65536u
       | OpTypeBool _ -> 65536u
       | OpTypeInt _ -> 65536u
       | OpTypeFloat _ -> 65536u
       | OpTypeVector _ -> 65536u
       | OpTypeMatrix _ -> 65536u
       | OpTypeImage _ -> 65536u
       | OpTypeSampler _ -> 65536u
       | OpTypeSampledImage _ -> 65536u
       | OpTypeArray _ -> 65536u
       | OpTypeRuntimeArray _ -> 65536u
       | OpTypeStruct _ -> 65536u
       | OpTypeOpaque _ -> 65536u
       | OpTypePointer _ -> 65536u
       | OpTypeFunction _ -> 65536u
       | OpTypeEvent _ -> 65536u
       | OpTypeDeviceEvent _ -> 65536u
       | OpTypeReserveId _ -> 65536u
       | OpTypeQueue _ -> 65536u
       | OpTypePipe _ -> 65536u
       | OpTypeForwardPointer _ -> 65536u
       | OpConstantTrue _ -> 65536u
       | OpConstantFalse _ -> 65536u
       | OpConstant _ -> 65536u
       | OpConstantComposite _ -> 65536u
       | OpConstantSampler _ -> 65536u
       | OpConstantNull _ -> 65536u
       | OpSpecConstantTrue _ -> 65536u
       | OpSpecConstantFalse _ -> 65536u
       | OpSpecConstant _ -> 65536u
       | OpSpecConstantComposite _ -> 65536u
       | OpSpecConstantOp _ -> 65536u
       | OpFunction _ -> 65536u
       | OpFunctionParameter _ -> 65536u
       | OpFunctionEnd -> 65536u
       | OpFunctionCall _ -> 65536u
       | OpVariable _ -> 65536u
       | OpImageTexelPointer _ -> 65536u
       | OpLoad _ -> 65536u
       | OpStore _ -> 65536u
       | OpCopyMemory _ -> 65536u
       | OpCopyMemorySized _ -> 65536u
       | OpAccessChain _ -> 65536u
       | OpInBoundsAccessChain _ -> 65536u
       | OpPtrAccessChain _ -> 65536u
       | OpArrayLength _ -> 65536u
       | OpGenericPtrMemSemantics _ -> 65536u
       | OpInBoundsPtrAccessChain _ -> 65536u
       | OpDecorate _ -> 65536u
       | OpMemberDecorate _ -> 65536u
       | OpDecorationGroup _ -> 65536u
       | OpGroupDecorate _ -> 65536u
       | OpGroupMemberDecorate _ -> 65536u
       | OpVectorExtractDynamic _ -> 65536u
       | OpVectorInsertDynamic _ -> 65536u
       | OpVectorShuffle _ -> 65536u
       | OpCompositeConstruct _ -> 65536u
       | OpCompositeExtract _ -> 65536u
       | OpCompositeInsert _ -> 65536u
       | OpCopyObject _ -> 65536u
       | OpTranspose _ -> 65536u
       | OpSampledImage _ -> 65536u
       | OpImageSampleImplicitLod _ -> 65536u
       | OpImageSampleExplicitLod _ -> 65536u
       | OpImageSampleDrefImplicitLod _ -> 65536u
       | OpImageSampleDrefExplicitLod _ -> 65536u
       | OpImageSampleProjImplicitLod _ -> 65536u
       | OpImageSampleProjExplicitLod _ -> 65536u
       | OpImageSampleProjDrefImplicitLod _ -> 65536u
       | OpImageSampleProjDrefExplicitLod _ -> 65536u
       | OpImageFetch _ -> 65536u
       | OpImageGather _ -> 65536u
       | OpImageDrefGather _ -> 65536u
       | OpImageRead _ -> 65536u
       | OpImageWrite _ -> 65536u
       | OpImage _ -> 65536u
       | OpImageQueryFormat _ -> 65536u
       | OpImageQueryOrder _ -> 65536u
       | OpImageQuerySizeLod _ -> 65536u
       | OpImageQuerySize _ -> 65536u
       | OpImageQueryLod _ -> 65536u
       | OpImageQueryLevels _ -> 65536u
       | OpImageQuerySamples _ -> 65536u
       | OpConvertFToU _ -> 65536u
       | OpConvertFToS _ -> 65536u
       | OpConvertSToF _ -> 65536u
       | OpConvertUToF _ -> 65536u
       | OpUConvert _ -> 65536u
       | OpSConvert _ -> 65536u
       | OpFConvert _ -> 65536u
       | OpQuantizeToF16 _ -> 65536u
       | OpConvertPtrToU _ -> 65536u
       | OpSatConvertSToU _ -> 65536u
       | OpSatConvertUToS _ -> 65536u
       | OpConvertUToPtr _ -> 65536u
       | OpPtrCastToGeneric _ -> 65536u
       | OpGenericCastToPtr _ -> 65536u
       | OpGenericCastToPtrExplicit _ -> 65536u
       | OpBitcast _ -> 65536u
       | OpSNegate _ -> 65536u
       | OpFNegate _ -> 65536u
       | OpIAdd _ -> 65536u
       | OpFAdd _ -> 65536u
       | OpISub _ -> 65536u
       | OpFSub _ -> 65536u
       | OpIMul _ -> 65536u
       | OpFMul _ -> 65536u
       | OpUDiv _ -> 65536u
       | OpSDiv _ -> 65536u
       | OpFDiv _ -> 65536u
       | OpUMod _ -> 65536u
       | OpSRem _ -> 65536u
       | OpSMod _ -> 65536u
       | OpFRem _ -> 65536u
       | OpFMod _ -> 65536u
       | OpVectorTimesScalar _ -> 65536u
       | OpMatrixTimesScalar _ -> 65536u
       | OpVectorTimesMatrix _ -> 65536u
       | OpMatrixTimesVector _ -> 65536u
       | OpMatrixTimesMatrix _ -> 65536u
       | OpOuterProduct _ -> 65536u
       | OpDot _ -> 65536u
       | OpIAddCarry _ -> 65536u
       | OpISubBorrow _ -> 65536u
       | OpUMulExtended _ -> 65536u
       | OpSMulExtended _ -> 65536u
       | OpAny _ -> 65536u
       | OpAll _ -> 65536u
       | OpIsNan _ -> 65536u
       | OpIsInf _ -> 65536u
       | OpIsFinite _ -> 65536u
       | OpIsNormal _ -> 65536u
       | OpSignBitSet _ -> 65536u
       | OpLessOrGreater _ -> 65536u
       | OpOrdered _ -> 65536u
       | OpUnordered _ -> 65536u
       | OpLogicalEqual _ -> 65536u
       | OpLogicalNotEqual _ -> 65536u
       | OpLogicalOr _ -> 65536u
       | OpLogicalAnd _ -> 65536u
       | OpLogicalNot _ -> 65536u
       | OpSelect _ -> 65536u
       | OpIEqual _ -> 65536u
       | OpINotEqual _ -> 65536u
       | OpUGreaterThan _ -> 65536u
       | OpSGreaterThan _ -> 65536u
       | OpUGreaterThanEqual _ -> 65536u
       | OpSGreaterThanEqual _ -> 65536u
       | OpULessThan _ -> 65536u
       | OpSLessThan _ -> 65536u
       | OpULessThanEqual _ -> 65536u
       | OpSLessThanEqual _ -> 65536u
       | OpFOrdEqual _ -> 65536u
       | OpFUnordEqual _ -> 65536u
       | OpFOrdNotEqual _ -> 65536u
       | OpFUnordNotEqual _ -> 65536u
       | OpFOrdLessThan _ -> 65536u
       | OpFUnordLessThan _ -> 65536u
       | OpFOrdGreaterThan _ -> 65536u
       | OpFUnordGreaterThan _ -> 65536u
       | OpFOrdLessThanEqual _ -> 65536u
       | OpFUnordLessThanEqual _ -> 65536u
       | OpFOrdGreaterThanEqual _ -> 65536u
       | OpFUnordGreaterThanEqual _ -> 65536u
       | OpShiftRightLogical _ -> 65536u
       | OpShiftRightArithmetic _ -> 65536u
       | OpShiftLeftLogical _ -> 65536u
       | OpBitwiseOr _ -> 65536u
       | OpBitwiseXor _ -> 65536u
       | OpBitwiseAnd _ -> 65536u
       | OpNot _ -> 65536u
       | OpBitFieldInsert _ -> 65536u
       | OpBitFieldSExtract _ -> 65536u
       | OpBitFieldUExtract _ -> 65536u
       | OpBitReverse _ -> 65536u
       | OpBitCount _ -> 65536u
       | OpDPdx _ -> 65536u
       | OpDPdy _ -> 65536u
       | OpFwidth _ -> 65536u
       | OpDPdxFine _ -> 65536u
       | OpDPdyFine _ -> 65536u
       | OpFwidthFine _ -> 65536u
       | OpDPdxCoarse _ -> 65536u
       | OpDPdyCoarse _ -> 65536u
       | OpFwidthCoarse _ -> 65536u
       | OpEmitVertex -> 65536u
       | OpEndPrimitive -> 65536u
       | OpEmitStreamVertex _ -> 65536u
       | OpEndStreamPrimitive _ -> 65536u
       | OpControlBarrier _ -> 65536u
       | OpMemoryBarrier _ -> 65536u
       | OpAtomicLoad _ -> 65536u
       | OpAtomicStore _ -> 65536u
       | OpAtomicExchange _ -> 65536u
       | OpAtomicCompareExchange _ -> 65536u
       | OpAtomicCompareExchangeWeak _ -> 65536u
       | OpAtomicIIncrement _ -> 65536u
       | OpAtomicIDecrement _ -> 65536u
       | OpAtomicIAdd _ -> 65536u
       | OpAtomicISub _ -> 65536u
       | OpAtomicSMin _ -> 65536u
       | OpAtomicUMin _ -> 65536u
       | OpAtomicSMax _ -> 65536u
       | OpAtomicUMax _ -> 65536u
       | OpAtomicAnd _ -> 65536u
       | OpAtomicOr _ -> 65536u
       | OpAtomicXor _ -> 65536u
       | OpPhi _ -> 65536u
       | OpLoopMerge _ -> 65536u
       | OpSelectionMerge _ -> 65536u
       | OpLabel _ -> 65536u
       | OpBranch _ -> 65536u
       | OpBranchConditional _ -> 65536u
       | OpSwitch _ -> 65536u
       | OpKill -> 65536u
       | OpReturn -> 65536u
       | OpReturnValue _ -> 65536u
       | OpUnreachable -> 65536u
       | OpLifetimeStart _ -> 65536u
       | OpLifetimeStop _ -> 65536u
       | OpGroupAsyncCopy _ -> 65536u
       | OpGroupWaitEvents _ -> 65536u
       | OpGroupAll _ -> 65536u
       | OpGroupAny _ -> 65536u
       | OpGroupBroadcast _ -> 65536u
       | OpGroupIAdd _ -> 65536u
       | OpGroupFAdd _ -> 65536u
       | OpGroupFMin _ -> 65536u
       | OpGroupUMin _ -> 65536u
       | OpGroupSMin _ -> 65536u
       | OpGroupFMax _ -> 65536u
       | OpGroupUMax _ -> 65536u
       | OpGroupSMax _ -> 65536u
       | OpReadPipe _ -> 65536u
       | OpWritePipe _ -> 65536u
       | OpReservedReadPipe _ -> 65536u
       | OpReservedWritePipe _ -> 65536u
       | OpReserveReadPipePackets _ -> 65536u
       | OpReserveWritePipePackets _ -> 65536u
       | OpCommitReadPipe _ -> 65536u
       | OpCommitWritePipe _ -> 65536u
       | OpIsValidReserveId _ -> 65536u
       | OpGetNumPipePackets _ -> 65536u
       | OpGetMaxPipePackets _ -> 65536u
       | OpGroupReserveReadPipePackets _ -> 65536u
       | OpGroupReserveWritePipePackets _ -> 65536u
       | OpGroupCommitReadPipe _ -> 65536u
       | OpGroupCommitWritePipe _ -> 65536u
       | OpEnqueueMarker _ -> 65536u
       | OpEnqueueKernel _ -> 65536u
       | OpGetKernelNDrangeSubGroupCount _ -> 65536u
       | OpGetKernelNDrangeMaxSubGroupSize _ -> 65536u
       | OpGetKernelWorkGroupSize _ -> 65536u
       | OpGetKernelPreferredWorkGroupSizeMultiple _ -> 65536u
       | OpRetainEvent _ -> 65536u
       | OpReleaseEvent _ -> 65536u
       | OpCreateUserEvent _ -> 65536u
       | OpIsValidEvent _ -> 65536u
       | OpSetUserEventStatus _ -> 65536u
       | OpCaptureEventProfilingInfo _ -> 65536u
       | OpGetDefaultQueue _ -> 65536u
       | OpBuildNDRange _ -> 65536u
       | OpImageSparseSampleImplicitLod _ -> 65536u
       | OpImageSparseSampleExplicitLod _ -> 65536u
       | OpImageSparseSampleDrefImplicitLod _ -> 65536u
       | OpImageSparseSampleDrefExplicitLod _ -> 65536u
       | OpImageSparseSampleProjImplicitLod _ -> 65536u
       | OpImageSparseSampleProjExplicitLod _ -> 65536u
       | OpImageSparseSampleProjDrefImplicitLod _ -> 65536u
       | OpImageSparseSampleProjDrefExplicitLod _ -> 65536u
       | OpImageSparseFetch _ -> 65536u
       | OpImageSparseGather _ -> 65536u
       | OpImageSparseDrefGather _ -> 65536u
       | OpImageSparseTexelsResident _ -> 65536u
       | OpNoLine -> 65536u
       | OpAtomicFlagTestAndSet _ -> 65536u
       | OpAtomicFlagClear _ -> 65536u
       | OpImageSparseRead _ -> 65536u
       | OpSizeOf _ -> 65792u
       | OpTypePipeStorage _ -> 65792u
       | OpConstantPipeStorage _ -> 65792u
       | OpCreatePipeFromPipeStorage _ -> 65792u
       | OpGetKernelLocalSizeForSubgroupCount _ -> 65792u
       | OpGetKernelMaxNumSubgroups _ -> 65792u
       | OpTypeNamedBarrier _ -> 65792u
       | OpNamedBarrierInitialize _ -> 65792u
       | OpMemoryNamedBarrier _ -> 65792u
       | OpModuleProcessed _ -> 65792u
       | OpExecutionModeId _ -> 66048u
       | OpDecorateId _ -> 66048u
       | OpGroupNonUniformElect _ -> 66304u
       | OpGroupNonUniformAll _ -> 66304u
       | OpGroupNonUniformAny _ -> 66304u
       | OpGroupNonUniformAllEqual _ -> 66304u
       | OpGroupNonUniformBroadcast _ -> 66304u
       | OpGroupNonUniformBroadcastFirst _ -> 66304u
       | OpGroupNonUniformBallot _ -> 66304u
       | OpGroupNonUniformInverseBallot _ -> 66304u
       | OpGroupNonUniformBallotBitExtract _ -> 66304u
       | OpGroupNonUniformBallotBitCount _ -> 66304u
       | OpGroupNonUniformBallotFindLSB _ -> 66304u
       | OpGroupNonUniformBallotFindMSB _ -> 66304u
       | OpGroupNonUniformShuffle _ -> 66304u
       | OpGroupNonUniformShuffleXor _ -> 66304u
       | OpGroupNonUniformShuffleUp _ -> 66304u
       | OpGroupNonUniformShuffleDown _ -> 66304u
       | OpGroupNonUniformIAdd _ -> 66304u
       | OpGroupNonUniformFAdd _ -> 66304u
       | OpGroupNonUniformIMul _ -> 66304u
       | OpGroupNonUniformFMul _ -> 66304u
       | OpGroupNonUniformSMin _ -> 66304u
       | OpGroupNonUniformUMin _ -> 66304u
       | OpGroupNonUniformFMin _ -> 66304u
       | OpGroupNonUniformSMax _ -> 66304u
       | OpGroupNonUniformUMax _ -> 66304u
       | OpGroupNonUniformFMax _ -> 66304u
       | OpGroupNonUniformBitwiseAnd _ -> 66304u
       | OpGroupNonUniformBitwiseOr _ -> 66304u
       | OpGroupNonUniformBitwiseXor _ -> 66304u
       | OpGroupNonUniformLogicalAnd _ -> 66304u
       | OpGroupNonUniformLogicalOr _ -> 66304u
       | OpGroupNonUniformLogicalXor _ -> 66304u
       | OpGroupNonUniformQuadBroadcast _ -> 66304u
       | OpGroupNonUniformQuadSwap _ -> 66304u
       | OpCopyLogical _ -> 66560u
       | OpPtrEqual _ -> 66560u
       | OpPtrNotEqual _ -> 66560u
       | OpPtrDiff _ -> 66560u
       | OpColorAttachmentReadEXT _ -> 65536u
       | OpDepthAttachmentReadEXT _ -> 65536u
       | OpStencilAttachmentReadEXT _ -> 65536u
       | OpTerminateInvocation -> 67072u
       | OpTypeUntypedPointerKHR _ -> 65536u
       | OpUntypedVariableKHR _ -> 65536u
       | OpUntypedAccessChainKHR _ -> 65536u
       | OpUntypedInBoundsAccessChainKHR _ -> 65536u
       | OpSubgroupBallotKHR _ -> 65536u
       | OpSubgroupFirstInvocationKHR _ -> 65536u
       | OpUntypedPtrAccessChainKHR _ -> 65536u
       | OpUntypedInBoundsPtrAccessChainKHR _ -> 65536u
       | OpUntypedArrayLengthKHR _ -> 65536u
       | OpUntypedPrefetchKHR _ -> 65536u
       | OpSubgroupAllKHR _ -> 65536u
       | OpSubgroupAnyKHR _ -> 65536u
       | OpSubgroupAllEqualKHR _ -> 65536u
       | OpGroupNonUniformRotateKHR _ -> 65536u
       | OpSubgroupReadInvocationKHR _ -> 65536u
       | OpExtInstWithForwardRefsKHR _ -> 65536u
       | OpTraceRayKHR _ -> 65536u
       | OpExecuteCallableKHR _ -> 65536u
       | OpConvertUToAccelerationStructureKHR _ -> 65536u
       | OpIgnoreIntersectionKHR -> 65536u
       | OpTerminateRayKHR -> 65536u
       | OpSDot _ -> 67072u
       | OpUDot _ -> 67072u
       | OpSUDot _ -> 67072u
       | OpSDotAccSat _ -> 67072u
       | OpUDotAccSat _ -> 67072u
       | OpSUDotAccSat _ -> 67072u
       | OpTypeCooperativeMatrixKHR _ -> 65536u
       | OpCooperativeMatrixLoadKHR _ -> 65536u
       | OpCooperativeMatrixStoreKHR _ -> 65536u
       | OpCooperativeMatrixMulAddKHR _ -> 65536u
       | OpCooperativeMatrixLengthKHR _ -> 65536u
       | OpConstantCompositeReplicateEXT _ -> 65536u
       | OpSpecConstantCompositeReplicateEXT _ -> 65536u
       | OpCompositeConstructReplicateEXT _ -> 65536u
       | OpTypeRayQueryKHR _ -> 65536u
       | OpRayQueryInitializeKHR _ -> 65536u
       | OpRayQueryTerminateKHR _ -> 65536u
       | OpRayQueryGenerateIntersectionKHR _ -> 65536u
       | OpRayQueryConfirmIntersectionKHR _ -> 65536u
       | OpRayQueryProceedKHR _ -> 65536u
       | OpRayQueryGetIntersectionTypeKHR _ -> 65536u
       | OpImageSampleWeightedQCOM _ -> 65536u
       | OpImageBoxFilterQCOM _ -> 65536u
       | OpImageBlockMatchSSDQCOM _ -> 65536u
       | OpImageBlockMatchSADQCOM _ -> 65536u
       | OpImageBlockMatchWindowSSDQCOM _ -> 65536u
       | OpImageBlockMatchWindowSADQCOM _ -> 65536u
       | OpImageBlockMatchGatherSSDQCOM _ -> 65536u
       | OpImageBlockMatchGatherSADQCOM _ -> 65536u
       | OpGroupIAddNonUniformAMD _ -> 65536u
       | OpGroupFAddNonUniformAMD _ -> 65536u
       | OpGroupFMinNonUniformAMD _ -> 65536u
       | OpGroupUMinNonUniformAMD _ -> 65536u
       | OpGroupSMinNonUniformAMD _ -> 65536u
       | OpGroupFMaxNonUniformAMD _ -> 65536u
       | OpGroupUMaxNonUniformAMD _ -> 65536u
       | OpGroupSMaxNonUniformAMD _ -> 65536u
       | OpFragmentMaskFetchAMD _ -> 65536u
       | OpFragmentFetchAMD _ -> 65536u
       | OpReadClockKHR _ -> 65536u
       | OpAllocateNodePayloadsAMDX _ -> 65536u
       | OpEnqueueNodePayloadsAMDX _ -> 65536u
       | OpTypeNodePayloadArrayAMDX _ -> 65536u
       | OpFinishWritingNodePayloadAMDX _ -> 65536u
       | OpNodePayloadArrayLengthAMDX _ -> 65536u
       | OpIsNodePayloadValidAMDX _ -> 65536u
       | OpConstantStringAMDX _ -> 65536u
       | OpSpecConstantStringAMDX _ -> 65536u
       | OpGroupNonUniformQuadAllKHR _ -> 65536u
       | OpGroupNonUniformQuadAnyKHR _ -> 65536u
       | OpHitObjectRecordHitMotionNV _ -> 65536u
       | OpHitObjectRecordHitWithIndexMotionNV _ -> 65536u
       | OpHitObjectRecordMissMotionNV _ -> 65536u
       | OpHitObjectGetWorldToObjectNV _ -> 65536u
       | OpHitObjectGetObjectToWorldNV _ -> 65536u
       | OpHitObjectGetObjectRayDirectionNV _ -> 65536u
       | OpHitObjectGetObjectRayOriginNV _ -> 65536u
       | OpHitObjectTraceRayMotionNV _ -> 65536u
       | OpHitObjectGetShaderRecordBufferHandleNV _ -> 65536u
       | OpHitObjectGetShaderBindingTableRecordIndexNV _ -> 65536u
       | OpHitObjectRecordEmptyNV _ -> 65536u
       | OpHitObjectTraceRayNV _ -> 65536u
       | OpHitObjectRecordHitNV _ -> 65536u
       | OpHitObjectRecordHitWithIndexNV _ -> 65536u
       | OpHitObjectRecordMissNV _ -> 65536u
       | OpHitObjectExecuteShaderNV _ -> 65536u
       | OpHitObjectGetCurrentTimeNV _ -> 65536u
       | OpHitObjectGetAttributesNV _ -> 65536u
       | OpHitObjectGetHitKindNV _ -> 65536u
       | OpHitObjectGetPrimitiveIndexNV _ -> 65536u
       | OpHitObjectGetGeometryIndexNV _ -> 65536u
       | OpHitObjectGetInstanceIdNV _ -> 65536u
       | OpHitObjectGetInstanceCustomIndexNV _ -> 65536u
       | OpHitObjectGetWorldRayDirectionNV _ -> 65536u
       | OpHitObjectGetWorldRayOriginNV _ -> 65536u
       | OpHitObjectGetRayTMaxNV _ -> 65536u
       | OpHitObjectGetRayTMinNV _ -> 65536u
       | OpHitObjectIsEmptyNV _ -> 65536u
       | OpHitObjectIsHitNV _ -> 65536u
       | OpHitObjectIsMissNV _ -> 65536u
       | OpReorderThreadWithHitObjectNV _ -> 65536u
       | OpReorderThreadWithHintNV _ -> 65536u
       | OpTypeHitObjectNV _ -> 65536u
       | OpImageSampleFootprintNV _ -> 65536u
       | OpCooperativeMatrixConvertNV _ -> 65536u
       | OpEmitMeshTasksEXT _ -> 65536u
       | OpSetMeshOutputsEXT _ -> 65536u
       | OpGroupNonUniformPartitionNV _ -> 65536u
       | OpWritePackedPrimitiveIndices4x8NV _ -> 65536u
       | OpFetchMicroTriangleVertexPositionNV _ -> 65536u
       | OpFetchMicroTriangleVertexBarycentricNV _ -> 65536u
       | OpReportIntersectionKHR _ -> 65536u
       | OpIgnoreIntersectionNV -> 65536u
       | OpTerminateRayNV -> 65536u
       | OpTraceNV _ -> 65536u
       | OpTraceMotionNV _ -> 65536u
       | OpTraceRayMotionNV _ -> 65536u
       | OpRayQueryGetIntersectionTriangleVertexPositionsKHR _ -> 65536u
       | OpTypeAccelerationStructureKHR _ -> 65536u
       | OpExecuteCallableNV _ -> 65536u
       | OpTypeCooperativeMatrixNV _ -> 65536u
       | OpCooperativeMatrixLoadNV _ -> 65536u
       | OpCooperativeMatrixStoreNV _ -> 65536u
       | OpCooperativeMatrixMulAddNV _ -> 65536u
       | OpCooperativeMatrixLengthNV _ -> 65536u
       | OpBeginInvocationInterlockEXT -> 65536u
       | OpEndInvocationInterlockEXT -> 65536u
       | OpCooperativeMatrixReduceNV _ -> 65536u
       | OpCooperativeMatrixLoadTensorNV _ -> 65536u
       | OpCooperativeMatrixStoreTensorNV _ -> 65536u
       | OpCooperativeMatrixPerElementOpNV _ -> 65536u
       | OpTypeTensorLayoutNV _ -> 65536u
       | OpTypeTensorViewNV _ -> 65536u
       | OpCreateTensorLayoutNV _ -> 65536u
       | OpTensorLayoutSetDimensionNV _ -> 65536u
       | OpTensorLayoutSetStrideNV _ -> 65536u
       | OpTensorLayoutSliceNV _ -> 65536u
       | OpTensorLayoutSetClampValueNV _ -> 65536u
       | OpCreateTensorViewNV _ -> 65536u
       | OpTensorViewSetDimensionNV _ -> 65536u
       | OpTensorViewSetStrideNV _ -> 65536u
       | OpDemoteToHelperInvocation -> 67072u
       | OpIsHelperInvocationEXT _ -> 65536u
       | OpTensorViewSetClipNV _ -> 65536u
       | OpTensorLayoutSetBlockSizeNV _ -> 65536u
       | OpCooperativeMatrixTransposeNV _ -> 65536u
       | OpConvertUToImageNV _ -> 65536u
       | OpConvertUToSamplerNV _ -> 65536u
       | OpConvertImageToUNV _ -> 65536u
       | OpConvertSamplerToUNV _ -> 65536u
       | OpConvertUToSampledImageNV _ -> 65536u
       | OpConvertSampledImageToUNV _ -> 65536u
       | OpSamplerImageAddressingModeNV _ -> 65536u
       | OpRawAccessChainNV _ -> 65536u
       | OpSubgroupShuffleINTEL _ -> 65536u
       | OpSubgroupShuffleDownINTEL _ -> 65536u
       | OpSubgroupShuffleUpINTEL _ -> 65536u
       | OpSubgroupShuffleXorINTEL _ -> 65536u
       | OpSubgroupBlockReadINTEL _ -> 65536u
       | OpSubgroupBlockWriteINTEL _ -> 65536u
       | OpSubgroupImageBlockReadINTEL _ -> 65536u
       | OpSubgroupImageBlockWriteINTEL _ -> 65536u
       | OpSubgroupImageMediaBlockReadINTEL _ -> 65536u
       | OpSubgroupImageMediaBlockWriteINTEL _ -> 65536u
       | OpUCountLeadingZerosINTEL _ -> 65536u
       | OpUCountTrailingZerosINTEL _ -> 65536u
       | OpAbsISubINTEL _ -> 65536u
       | OpAbsUSubINTEL _ -> 65536u
       | OpIAddSatINTEL _ -> 65536u
       | OpUAddSatINTEL _ -> 65536u
       | OpIAverageINTEL _ -> 65536u
       | OpUAverageINTEL _ -> 65536u
       | OpIAverageRoundedINTEL _ -> 65536u
       | OpUAverageRoundedINTEL _ -> 65536u
       | OpISubSatINTEL _ -> 65536u
       | OpUSubSatINTEL _ -> 65536u
       | OpIMul32x16INTEL _ -> 65536u
       | OpUMul32x16INTEL _ -> 65536u
       | OpConstantFunctionPointerINTEL _ -> 65536u
       | OpFunctionPointerCallINTEL _ -> 65536u
       | OpAsmTargetINTEL _ -> 65536u
       | OpAsmINTEL _ -> 65536u
       | OpAsmCallINTEL _ -> 65536u
       | OpAtomicFMinEXT _ -> 65536u
       | OpAtomicFMaxEXT _ -> 65536u
       | OpAssumeTrueKHR _ -> 65536u
       | OpExpectKHR _ -> 65536u
       | OpDecorateString _ -> 66560u
       | OpMemberDecorateString _ -> 66560u
       | OpVmeImageINTEL _ -> 65536u
       | OpTypeVmeImageINTEL _ -> 65536u
       | OpTypeAvcImePayloadINTEL _ -> 65536u
       | OpTypeAvcRefPayloadINTEL _ -> 65536u
       | OpTypeAvcSicPayloadINTEL _ -> 65536u
       | OpTypeAvcMcePayloadINTEL _ -> 65536u
       | OpTypeAvcMceResultINTEL _ -> 65536u
       | OpTypeAvcImeResultINTEL _ -> 65536u
       | OpTypeAvcImeResultSingleReferenceStreamoutINTEL _ -> 65536u
       | OpTypeAvcImeResultDualReferenceStreamoutINTEL _ -> 65536u
       | OpTypeAvcImeSingleReferenceStreaminINTEL _ -> 65536u
       | OpTypeAvcImeDualReferenceStreaminINTEL _ -> 65536u
       | OpTypeAvcRefResultINTEL _ -> 65536u
       | OpTypeAvcSicResultINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultInterBaseMultiReferencePenaltyINTEL _ -> 65536u
       | OpSubgroupAvcMceSetInterBaseMultiReferencePenaltyINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultInterShapePenaltyINTEL _ -> 65536u
       | OpSubgroupAvcMceSetInterShapePenaltyINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultInterDirectionPenaltyINTEL _ -> 65536u
       | OpSubgroupAvcMceSetInterDirectionPenaltyINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultIntraLumaShapePenaltyINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultInterMotionVectorCostTableINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultHighPenaltyCostTableINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultMediumPenaltyCostTableINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultLowPenaltyCostTableINTEL _ -> 65536u
       | OpSubgroupAvcMceSetMotionVectorCostFunctionINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultIntraLumaModePenaltyINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultNonDcLumaIntraPenaltyINTEL _ -> 65536u
       | OpSubgroupAvcMceGetDefaultIntraChromaModeBasePenaltyINTEL _ -> 65536u
       | OpSubgroupAvcMceSetAcOnlyHaarINTEL _ -> 65536u
       | OpSubgroupAvcMceSetSourceInterlacedFieldPolarityINTEL _ -> 65536u
       | OpSubgroupAvcMceSetSingleReferenceInterlacedFieldPolarityINTEL _ -> 65536u
       | OpSubgroupAvcMceSetDualReferenceInterlacedFieldPolaritiesINTEL _ -> 65536u
       | OpSubgroupAvcMceConvertToImePayloadINTEL _ -> 65536u
       | OpSubgroupAvcMceConvertToImeResultINTEL _ -> 65536u
       | OpSubgroupAvcMceConvertToRefPayloadINTEL _ -> 65536u
       | OpSubgroupAvcMceConvertToRefResultINTEL _ -> 65536u
       | OpSubgroupAvcMceConvertToSicPayloadINTEL _ -> 65536u
       | OpSubgroupAvcMceConvertToSicResultINTEL _ -> 65536u
       | OpSubgroupAvcMceGetMotionVectorsINTEL _ -> 65536u
       | OpSubgroupAvcMceGetInterDistortionsINTEL _ -> 65536u
       | OpSubgroupAvcMceGetBestInterDistortionsINTEL _ -> 65536u
       | OpSubgroupAvcMceGetInterMajorShapeINTEL _ -> 65536u
       | OpSubgroupAvcMceGetInterMinorShapeINTEL _ -> 65536u
       | OpSubgroupAvcMceGetInterDirectionsINTEL _ -> 65536u
       | OpSubgroupAvcMceGetInterMotionVectorCountINTEL _ -> 65536u
       | OpSubgroupAvcMceGetInterReferenceIdsINTEL _ -> 65536u
       | OpSubgroupAvcMceGetInterReferenceInterlacedFieldPolaritiesINTEL _ -> 65536u
       | OpSubgroupAvcImeInitializeINTEL _ -> 65536u
       | OpSubgroupAvcImeSetSingleReferenceINTEL _ -> 65536u
       | OpSubgroupAvcImeSetDualReferenceINTEL _ -> 65536u
       | OpSubgroupAvcImeRefWindowSizeINTEL _ -> 65536u
       | OpSubgroupAvcImeAdjustRefOffsetINTEL _ -> 65536u
       | OpSubgroupAvcImeConvertToMcePayloadINTEL _ -> 65536u
       | OpSubgroupAvcImeSetMaxMotionVectorCountINTEL _ -> 65536u
       | OpSubgroupAvcImeSetUnidirectionalMixDisableINTEL _ -> 65536u
       | OpSubgroupAvcImeSetEarlySearchTerminationThresholdINTEL _ -> 65536u
       | OpSubgroupAvcImeSetWeightedSadINTEL _ -> 65536u
       | OpSubgroupAvcImeEvaluateWithSingleReferenceINTEL _ -> 65536u
       | OpSubgroupAvcImeEvaluateWithDualReferenceINTEL _ -> 65536u
       | OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminINTEL _ -> 65536u
       | OpSubgroupAvcImeEvaluateWithDualReferenceStreaminINTEL _ -> 65536u
       | OpSubgroupAvcImeEvaluateWithSingleReferenceStreamoutINTEL _ -> 65536u
       | OpSubgroupAvcImeEvaluateWithDualReferenceStreamoutINTEL _ -> 65536u
       | OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminoutINTEL _ -> 65536u
       | OpSubgroupAvcImeEvaluateWithDualReferenceStreaminoutINTEL _ -> 65536u
       | OpSubgroupAvcImeConvertToMceResultINTEL _ -> 65536u
       | OpSubgroupAvcImeGetSingleReferenceStreaminINTEL _ -> 65536u
       | OpSubgroupAvcImeGetDualReferenceStreaminINTEL _ -> 65536u
       | OpSubgroupAvcImeStripSingleReferenceStreamoutINTEL _ -> 65536u
       | OpSubgroupAvcImeStripDualReferenceStreamoutINTEL _ -> 65536u
       | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeMotionVectorsINTEL _ -> 65536u
       | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeDistortionsINTEL _ -> 65536u
       | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeReferenceIdsINTEL _ -> 65536u
       | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeMotionVectorsINTEL _ -> 65536u
       | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeDistortionsINTEL _ -> 65536u
       | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeReferenceIdsINTEL _ -> 65536u
       | OpSubgroupAvcImeGetBorderReachedINTEL _ -> 65536u
       | OpSubgroupAvcImeGetTruncatedSearchIndicationINTEL _ -> 65536u
       | OpSubgroupAvcImeGetUnidirectionalEarlySearchTerminationINTEL _ -> 65536u
       | OpSubgroupAvcImeGetWeightingPatternMinimumMotionVectorINTEL _ -> 65536u
       | OpSubgroupAvcImeGetWeightingPatternMinimumDistortionINTEL _ -> 65536u
       | OpSubgroupAvcFmeInitializeINTEL _ -> 65536u
       | OpSubgroupAvcBmeInitializeINTEL _ -> 65536u
       | OpSubgroupAvcRefConvertToMcePayloadINTEL _ -> 65536u
       | OpSubgroupAvcRefSetBidirectionalMixDisableINTEL _ -> 65536u
       | OpSubgroupAvcRefSetBilinearFilterEnableINTEL _ -> 65536u
       | OpSubgroupAvcRefEvaluateWithSingleReferenceINTEL _ -> 65536u
       | OpSubgroupAvcRefEvaluateWithDualReferenceINTEL _ -> 65536u
       | OpSubgroupAvcRefEvaluateWithMultiReferenceINTEL _ -> 65536u
       | OpSubgroupAvcRefEvaluateWithMultiReferenceInterlacedINTEL _ -> 65536u
       | OpSubgroupAvcRefConvertToMceResultINTEL _ -> 65536u
       | OpSubgroupAvcSicInitializeINTEL _ -> 65536u
       | OpSubgroupAvcSicConfigureSkcINTEL _ -> 65536u
       | OpSubgroupAvcSicConfigureIpeLumaINTEL _ -> 65536u
       | OpSubgroupAvcSicConfigureIpeLumaChromaINTEL _ -> 65536u
       | OpSubgroupAvcSicGetMotionVectorMaskINTEL _ -> 65536u
       | OpSubgroupAvcSicConvertToMcePayloadINTEL _ -> 65536u
       | OpSubgroupAvcSicSetIntraLumaShapePenaltyINTEL _ -> 65536u
       | OpSubgroupAvcSicSetIntraLumaModeCostFunctionINTEL _ -> 65536u
       | OpSubgroupAvcSicSetIntraChromaModeCostFunctionINTEL _ -> 65536u
       | OpSubgroupAvcSicSetBilinearFilterEnableINTEL _ -> 65536u
       | OpSubgroupAvcSicSetSkcForwardTransformEnableINTEL _ -> 65536u
       | OpSubgroupAvcSicSetBlockBasedRawSkipSadINTEL _ -> 65536u
       | OpSubgroupAvcSicEvaluateIpeINTEL _ -> 65536u
       | OpSubgroupAvcSicEvaluateWithSingleReferenceINTEL _ -> 65536u
       | OpSubgroupAvcSicEvaluateWithDualReferenceINTEL _ -> 65536u
       | OpSubgroupAvcSicEvaluateWithMultiReferenceINTEL _ -> 65536u
       | OpSubgroupAvcSicEvaluateWithMultiReferenceInterlacedINTEL _ -> 65536u
       | OpSubgroupAvcSicConvertToMceResultINTEL _ -> 65536u
       | OpSubgroupAvcSicGetIpeLumaShapeINTEL _ -> 65536u
       | OpSubgroupAvcSicGetBestIpeLumaDistortionINTEL _ -> 65536u
       | OpSubgroupAvcSicGetBestIpeChromaDistortionINTEL _ -> 65536u
       | OpSubgroupAvcSicGetPackedIpeLumaModesINTEL _ -> 65536u
       | OpSubgroupAvcSicGetIpeChromaModeINTEL _ -> 65536u
       | OpSubgroupAvcSicGetPackedSkcLumaCountThresholdINTEL _ -> 65536u
       | OpSubgroupAvcSicGetPackedSkcLumaSumThresholdINTEL _ -> 65536u
       | OpSubgroupAvcSicGetInterRawSadsINTEL _ -> 65536u
       | OpVariableLengthArrayINTEL _ -> 65536u
       | OpSaveMemoryINTEL _ -> 65536u
       | OpRestoreMemoryINTEL _ -> 65536u
       | OpArbitraryFloatSinCosPiINTEL _ -> 65536u
       | OpArbitraryFloatCastINTEL _ -> 65536u
       | OpArbitraryFloatCastFromIntINTEL _ -> 65536u
       | OpArbitraryFloatCastToIntINTEL _ -> 65536u
       | OpArbitraryFloatAddINTEL _ -> 65536u
       | OpArbitraryFloatSubINTEL _ -> 65536u
       | OpArbitraryFloatMulINTEL _ -> 65536u
       | OpArbitraryFloatDivINTEL _ -> 65536u
       | OpArbitraryFloatGTINTEL _ -> 65536u
       | OpArbitraryFloatGEINTEL _ -> 65536u
       | OpArbitraryFloatLTINTEL _ -> 65536u
       | OpArbitraryFloatLEINTEL _ -> 65536u
       | OpArbitraryFloatEQINTEL _ -> 65536u
       | OpArbitraryFloatRecipINTEL _ -> 65536u
       | OpArbitraryFloatRSqrtINTEL _ -> 65536u
       | OpArbitraryFloatCbrtINTEL _ -> 65536u
       | OpArbitraryFloatHypotINTEL _ -> 65536u
       | OpArbitraryFloatSqrtINTEL _ -> 65536u
       | OpArbitraryFloatLogINTEL _ -> 65536u
       | OpArbitraryFloatLog2INTEL _ -> 65536u
       | OpArbitraryFloatLog10INTEL _ -> 65536u
       | OpArbitraryFloatLog1pINTEL _ -> 65536u
       | OpArbitraryFloatExpINTEL _ -> 65536u
       | OpArbitraryFloatExp2INTEL _ -> 65536u
       | OpArbitraryFloatExp10INTEL _ -> 65536u
       | OpArbitraryFloatExpm1INTEL _ -> 65536u
       | OpArbitraryFloatSinINTEL _ -> 65536u
       | OpArbitraryFloatCosINTEL _ -> 65536u
       | OpArbitraryFloatSinCosINTEL _ -> 65536u
       | OpArbitraryFloatSinPiINTEL _ -> 65536u
       | OpArbitraryFloatCosPiINTEL _ -> 65536u
       | OpArbitraryFloatASinINTEL _ -> 65536u
       | OpArbitraryFloatASinPiINTEL _ -> 65536u
       | OpArbitraryFloatACosINTEL _ -> 65536u
       | OpArbitraryFloatACosPiINTEL _ -> 65536u
       | OpArbitraryFloatATanINTEL _ -> 65536u
       | OpArbitraryFloatATanPiINTEL _ -> 65536u
       | OpArbitraryFloatATan2INTEL _ -> 65536u
       | OpArbitraryFloatPowINTEL _ -> 65536u
       | OpArbitraryFloatPowRINTEL _ -> 65536u
       | OpArbitraryFloatPowNINTEL _ -> 65536u
       | OpLoopControlINTEL _ -> 65536u
       | OpAliasDomainDeclINTEL _ -> 65536u
       | OpAliasScopeDeclINTEL _ -> 65536u
       | OpAliasScopeListDeclINTEL _ -> 65536u
       | OpFixedSqrtINTEL _ -> 65536u
       | OpFixedRecipINTEL _ -> 65536u
       | OpFixedRsqrtINTEL _ -> 65536u
       | OpFixedSinINTEL _ -> 65536u
       | OpFixedCosINTEL _ -> 65536u
       | OpFixedSinCosINTEL _ -> 65536u
       | OpFixedSinPiINTEL _ -> 65536u
       | OpFixedCosPiINTEL _ -> 65536u
       | OpFixedSinCosPiINTEL _ -> 65536u
       | OpFixedLogINTEL _ -> 65536u
       | OpFixedExpINTEL _ -> 65536u
       | OpPtrCastToCrossWorkgroupINTEL _ -> 65536u
       | OpCrossWorkgroupCastToPtrINTEL _ -> 65536u
       | OpReadPipeBlockingINTEL _ -> 65536u
       | OpWritePipeBlockingINTEL _ -> 65536u
       | OpFPGARegINTEL _ -> 65536u
       | OpRayQueryGetRayTMinKHR _ -> 65536u
       | OpRayQueryGetRayFlagsKHR _ -> 65536u
       | OpRayQueryGetIntersectionTKHR _ -> 65536u
       | OpRayQueryGetIntersectionInstanceCustomIndexKHR _ -> 65536u
       | OpRayQueryGetIntersectionInstanceIdKHR _ -> 65536u
       | OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR _ -> 65536u
       | OpRayQueryGetIntersectionGeometryIndexKHR _ -> 65536u
       | OpRayQueryGetIntersectionPrimitiveIndexKHR _ -> 65536u
       | OpRayQueryGetIntersectionBarycentricsKHR _ -> 65536u
       | OpRayQueryGetIntersectionFrontFaceKHR _ -> 65536u
       | OpRayQueryGetIntersectionCandidateAABBOpaqueKHR _ -> 65536u
       | OpRayQueryGetIntersectionObjectRayDirectionKHR _ -> 65536u
       | OpRayQueryGetIntersectionObjectRayOriginKHR _ -> 65536u
       | OpRayQueryGetWorldRayDirectionKHR _ -> 65536u
       | OpRayQueryGetWorldRayOriginKHR _ -> 65536u
       | OpRayQueryGetIntersectionObjectToWorldKHR _ -> 65536u
       | OpRayQueryGetIntersectionWorldToObjectKHR _ -> 65536u
       | OpAtomicFAddEXT _ -> 65536u
       | OpTypeBufferSurfaceINTEL _ -> 65536u
       | OpTypeStructContinuedINTEL _ -> 65536u
       | OpConstantCompositeContinuedINTEL _ -> 65536u
       | OpSpecConstantCompositeContinuedINTEL _ -> 65536u
       | OpCompositeConstructContinuedINTEL _ -> 65536u
       | OpConvertFToBF16INTEL _ -> 65536u
       | OpConvertBF16ToFINTEL _ -> 65536u
       | OpControlBarrierArriveINTEL _ -> 65536u
       | OpControlBarrierWaitINTEL _ -> 65536u
       | OpArithmeticFenceEXT _ -> 65536u
       | OpSubgroupBlockPrefetchINTEL _ -> 65536u
       | OpGroupIMulKHR _ -> 65536u
       | OpGroupFMulKHR _ -> 65536u
       | OpGroupBitwiseAndKHR _ -> 65536u
       | OpGroupBitwiseOrKHR _ -> 65536u
       | OpGroupBitwiseXorKHR _ -> 65536u
       | OpGroupLogicalAndKHR _ -> 65536u
       | OpGroupLogicalOrKHR _ -> 65536u
       | OpGroupLogicalXorKHR _ -> 65536u
       | OpMaskedGatherINTEL _ -> 65536u
       | OpMaskedScatterINTEL _ -> 65536u

    static member internal Serialize(instr: Instruction, stream: SpirvStream) =
        match instr with
        | OpNop ->
            ()
        | OpUndef(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpSourceContinued(arg0) ->
            stream.WriteString(arg0)
        | OpSource(arg0, arg1, arg2, arg3) ->
            stream.WriteEnum(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteOption(arg2, fun v -> stream.WriteUInt32(v))
            stream.WriteOption(arg3, fun v -> stream.WriteString(v))
        | OpSourceExtension(arg0) ->
            stream.WriteString(arg0)
        | OpName(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteString(arg1)
        | OpMemberName(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteString(arg2)
        | OpString(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteString(arg1)
        | OpLine(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpExtension(arg0) ->
            stream.WriteString(arg0)
        | OpExtInstImport(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteString(arg1)
        | OpExtInst(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteList(arg4, fun v -> stream.WriteUInt32(v))
        | OpMemoryModel(arg0, arg1) ->
            stream.WriteEnum(arg0)
            stream.WriteEnum(arg1)
        | OpEntryPoint(arg0, arg1, arg2, arg3) ->
            stream.WriteEnum(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteString(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpExecutionMode(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            
            match arg1 with
            | ExecutionMode.Invocations(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SpacingEqual -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SpacingFractionalEven -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SpacingFractionalOdd -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.VertexOrderCw -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.VertexOrderCcw -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PixelCenterInteger -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OriginUpperLeft -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OriginLowerLeft -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.EarlyFragmentTests -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PointMode -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Xfb -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DepthReplacing -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DepthGreater -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DepthLess -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DepthUnchanged -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.LocalSize(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.LocalSizeHint(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.InputPoints -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.InputLines -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.InputLinesAdjacency -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Triangles -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.InputTrianglesAdjacency -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Quads -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Isolines -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputVertices(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.OutputPoints -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputLineStrip -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputTriangleStrip -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.VecTypeHint(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.ContractionOff -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Initializer -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Finalizer -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SubgroupSize(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SubgroupsPerWorkgroup(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SubgroupsPerWorkgroupId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.LocalSizeId(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.LocalSizeHintId(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.NonCoherentColorAttachmentReadEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.NonCoherentDepthAttachmentReadEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.NonCoherentStencilAttachmentReadEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SubgroupUniformControlFlowKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PostDepthCoverage -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DenormPreserve(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.DenormFlushToZero(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SignedZeroInfNanPreserve(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTE(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTZ(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.EarlyAndLateFragmentTestsAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefReplacingEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.CoalescingAMDX -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.IsApiEntryAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaxNodeRecursionAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.StaticNumWorkgroupsAMDX(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.ShaderIndexAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaxNumWorkgroupsAMDX(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.StencilRefUnchangedFrontAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefGreaterFrontAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefLessFrontAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefUnchangedBackAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefGreaterBackAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefLessBackAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.QuadDerivativesKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.RequireFullQuadsKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SharesInputWithAMDX(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1)
            | ExecutionMode.OutputLinesEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputPrimitivesEXT(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.DerivativeGroupQuadsKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DerivativeGroupLinearKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputTrianglesEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PixelInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PixelInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SampleInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SampleInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.ShadingRateInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.ShadingRateInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SharedLocalMemorySizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTPINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTNINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.FloatingPointModeALTINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.FloatingPointModeIEEEINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaxWorkgroupSizeINTEL(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.MaxWorkDimINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.NoGlobalOffsetINTEL -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.NumSIMDWorkitemsINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SchedulerTargetFmaxMhzINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaximallyReconvergesKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.FPFastMathDefault(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1)
            | ExecutionMode.StreamingInterfaceINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RegisterMapInterfaceINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.NamedBarrierCountINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaximumRegistersINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaximumRegistersIdINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.NamedMaximumRegistersINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
        | OpCapability(arg0) ->
            stream.WriteEnum(arg0)
        | OpTypeVoid(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeBool(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeInt(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpTypeFloat(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteOption(arg2, fun v -> stream.WriteEnum(v))
        | OpTypeVector(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpTypeMatrix(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpTypeImage(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteEnum(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteEnum(arg7)
            stream.WriteOption(arg8, fun v -> stream.WriteEnum(v))
        | OpTypeSampler(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeSampledImage(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpTypeArray(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpTypeRuntimeArray(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpTypeStruct(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteList(arg1, fun v -> stream.WriteUInt32(v))
        | OpTypeOpaque(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteString(arg1)
        | OpTypePointer(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteEnum(arg1)
            stream.WriteUInt32(arg2)
        | OpTypeFunction(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteList(arg2, fun v -> stream.WriteUInt32(v))
        | OpTypeEvent(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeDeviceEvent(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeReserveId(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeQueue(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypePipe(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteEnum(arg1)
        | OpTypeForwardPointer(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteEnum(arg1)
        | OpConstantTrue(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpConstantFalse(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpConstant(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConstantComposite(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteList(arg2, fun v -> stream.WriteUInt32(v))
        | OpConstantSampler(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteEnum(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteEnum(arg4)
        | OpConstantNull(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpSpecConstantTrue(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpSpecConstantFalse(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpSpecConstant(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSpecConstantComposite(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteList(arg2, fun v -> stream.WriteUInt32(v))
        | OpSpecConstantOp(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpFunction(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteEnum(arg2)
            stream.WriteUInt32(arg3)
        | OpFunctionParameter(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpFunctionEnd ->
            ()
        | OpFunctionCall(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpVariable(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteEnum(arg2)
            stream.WriteOption(arg3, fun v -> stream.WriteUInt32(v))
        | OpImageTexelPointer(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpLoad(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteOption(arg3, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpStore(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteOption(arg2, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpCopyMemory(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteOption(arg2, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
            stream.WriteOption(arg3, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpCopyMemorySized(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteOption(arg3, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
            stream.WriteOption(arg4, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpAccessChain(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpInBoundsAccessChain(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpPtrAccessChain(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteList(arg4, fun v -> stream.WriteUInt32(v))
        | OpArrayLength(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGenericPtrMemSemantics(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpInBoundsPtrAccessChain(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteList(arg4, fun v -> stream.WriteUInt32(v))
        | OpDecorate(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            
            match arg1 with
            | Decoration.RelaxedPrecision -> stream.WriteUInt32(arg1.Value)
            | Decoration.SpecId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Block -> stream.WriteUInt32(arg1.Value)
            | Decoration.BufferBlock -> stream.WriteUInt32(arg1.Value)
            | Decoration.RowMajor -> stream.WriteUInt32(arg1.Value)
            | Decoration.ColMajor -> stream.WriteUInt32(arg1.Value)
            | Decoration.ArrayStride(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MatrixStride(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.GLSLShared -> stream.WriteUInt32(arg1.Value)
            | Decoration.GLSLPacked -> stream.WriteUInt32(arg1.Value)
            | Decoration.CPacked -> stream.WriteUInt32(arg1.Value)
            | Decoration.BuiltIn(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.NoPerspective -> stream.WriteUInt32(arg1.Value)
            | Decoration.Flat -> stream.WriteUInt32(arg1.Value)
            | Decoration.Patch -> stream.WriteUInt32(arg1.Value)
            | Decoration.Centroid -> stream.WriteUInt32(arg1.Value)
            | Decoration.Sample -> stream.WriteUInt32(arg1.Value)
            | Decoration.Invariant -> stream.WriteUInt32(arg1.Value)
            | Decoration.Restrict -> stream.WriteUInt32(arg1.Value)
            | Decoration.Aliased -> stream.WriteUInt32(arg1.Value)
            | Decoration.Volatile -> stream.WriteUInt32(arg1.Value)
            | Decoration.Constant -> stream.WriteUInt32(arg1.Value)
            | Decoration.Coherent -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonWritable -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonReadable -> stream.WriteUInt32(arg1.Value)
            | Decoration.Uniform -> stream.WriteUInt32(arg1.Value)
            | Decoration.UniformId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.SaturatedConversion -> stream.WriteUInt32(arg1.Value)
            | Decoration.Stream(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Location(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Component(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Index(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Binding(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.DescriptorSet(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Offset(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.XfbBuffer(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.XfbStride(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.FuncParamAttr(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.FPRoundingMode(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.FPFastMathMode(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.LinkageAttributes(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.NoContraction -> stream.WriteUInt32(arg1.Value)
            | Decoration.InputAttachmentIndex(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Alignment(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxByteOffset(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.AlignmentId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxByteOffsetId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.NoSignedWrap -> stream.WriteUInt32(arg1.Value)
            | Decoration.NoUnsignedWrap -> stream.WriteUInt32(arg1.Value)
            | Decoration.WeightTextureQCOM -> stream.WriteUInt32(arg1.Value)
            | Decoration.BlockMatchTextureQCOM -> stream.WriteUInt32(arg1.Value)
            | Decoration.BlockMatchSamplerQCOM -> stream.WriteUInt32(arg1.Value)
            | Decoration.ExplicitInterpAMD -> stream.WriteUInt32(arg1.Value)
            | Decoration.NodeSharesPayloadLimitsWithAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.NodeMaxPayloadsAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.TrackFinishWritingAMDX -> stream.WriteUInt32(arg1.Value)
            | Decoration.PayloadNodeNameAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PayloadNodeBaseIndexAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PayloadNodeSparseArrayAMDX -> stream.WriteUInt32(arg1.Value)
            | Decoration.PayloadNodeArraySizeAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PayloadDispatchIndirectAMDX -> stream.WriteUInt32(arg1.Value)
            | Decoration.OverrideCoverageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PassthroughNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.ViewportRelativeNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.SecondaryViewportRelativeNV(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PerPrimitiveEXT -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerViewNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerTaskNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerVertexKHR -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonUniform -> stream.WriteUInt32(arg1.Value)
            | Decoration.RestrictPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.AliasedPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.HitObjectShaderRecordBufferNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BindlessSamplerNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BindlessImageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BoundSamplerNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BoundImageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.SIMTCallINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.ReferencedIndirectlyINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.ClobberINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.SideEffectsINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.VectorComputeVariableINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.FuncParamIOKindINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.VectorComputeFunctionINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.StackCallINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.GlobalVariableOffsetINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.CounterBuffer(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.UserSemantic(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.UserTypeGOOGLE(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.FunctionRoundingModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.FunctionDenormModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.RegisterINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MemoryINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.NumbanksINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.BankwidthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxPrivateCopiesINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.SinglepumpINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.DoublepumpINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MaxReplicatesINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.SimpleDualPortINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MergeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0);stream.WriteString(arg1_arg1)
            | Decoration.BankBitsINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.ForcePow2DepthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.StridesizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.WordsizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.TrueDualPortINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.BurstCoalesceINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.CacheSizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.DontStaticallyCoalesceINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.PrefetchINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.StallEnableINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.FuseLoopsInFunctionINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MathOpDSPModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1)
            | Decoration.AliasScopeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.NoAliasINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.InitiationIntervalINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxConcurrencyINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PipelineEnableINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.BufferLocationINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.IOPipeStorageINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.FunctionFloatingPointModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.SingleElementVectorINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.VectorComputeCallableFunctionINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MediaBlockIOINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.StallFreeINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.FPMaxErrorDecorationINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.LatencyControlLabelINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.LatencyControlConstraintINTEL(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | Decoration.ConduitKernelArgumentINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.RegisterMapKernelArgumentINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MMHostInterfaceAddressWidthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceDataWidthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceLatencyINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceReadWriteModeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.MMHostInterfaceMaxBurstINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceWaitRequestINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.StableKernelArgumentINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.HostAccessINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0);stream.WriteString(arg1_arg1)
            | Decoration.InitModeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.ImplementInRegisterMapINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.CacheControlLoadINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.CacheControlStoreINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
        | OpMemberDecorate(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            
            match arg2 with
            | Decoration.RelaxedPrecision -> stream.WriteUInt32(arg2.Value)
            | Decoration.SpecId(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Block -> stream.WriteUInt32(arg2.Value)
            | Decoration.BufferBlock -> stream.WriteUInt32(arg2.Value)
            | Decoration.RowMajor -> stream.WriteUInt32(arg2.Value)
            | Decoration.ColMajor -> stream.WriteUInt32(arg2.Value)
            | Decoration.ArrayStride(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MatrixStride(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.GLSLShared -> stream.WriteUInt32(arg2.Value)
            | Decoration.GLSLPacked -> stream.WriteUInt32(arg2.Value)
            | Decoration.CPacked -> stream.WriteUInt32(arg2.Value)
            | Decoration.BuiltIn(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.NoPerspective -> stream.WriteUInt32(arg2.Value)
            | Decoration.Flat -> stream.WriteUInt32(arg2.Value)
            | Decoration.Patch -> stream.WriteUInt32(arg2.Value)
            | Decoration.Centroid -> stream.WriteUInt32(arg2.Value)
            | Decoration.Sample -> stream.WriteUInt32(arg2.Value)
            | Decoration.Invariant -> stream.WriteUInt32(arg2.Value)
            | Decoration.Restrict -> stream.WriteUInt32(arg2.Value)
            | Decoration.Aliased -> stream.WriteUInt32(arg2.Value)
            | Decoration.Volatile -> stream.WriteUInt32(arg2.Value)
            | Decoration.Constant -> stream.WriteUInt32(arg2.Value)
            | Decoration.Coherent -> stream.WriteUInt32(arg2.Value)
            | Decoration.NonWritable -> stream.WriteUInt32(arg2.Value)
            | Decoration.NonReadable -> stream.WriteUInt32(arg2.Value)
            | Decoration.Uniform -> stream.WriteUInt32(arg2.Value)
            | Decoration.UniformId(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.SaturatedConversion -> stream.WriteUInt32(arg2.Value)
            | Decoration.Stream(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Location(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Component(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Index(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Binding(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.DescriptorSet(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Offset(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.XfbBuffer(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.XfbStride(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.FuncParamAttr(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.FPRoundingMode(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.FPFastMathMode(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.LinkageAttributes(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0);stream.WriteEnum(arg2_arg1)
            | Decoration.NoContraction -> stream.WriteUInt32(arg2.Value)
            | Decoration.InputAttachmentIndex(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Alignment(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MaxByteOffset(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.AlignmentId(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MaxByteOffsetId(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.NoSignedWrap -> stream.WriteUInt32(arg2.Value)
            | Decoration.NoUnsignedWrap -> stream.WriteUInt32(arg2.Value)
            | Decoration.WeightTextureQCOM -> stream.WriteUInt32(arg2.Value)
            | Decoration.BlockMatchTextureQCOM -> stream.WriteUInt32(arg2.Value)
            | Decoration.BlockMatchSamplerQCOM -> stream.WriteUInt32(arg2.Value)
            | Decoration.ExplicitInterpAMD -> stream.WriteUInt32(arg2.Value)
            | Decoration.NodeSharesPayloadLimitsWithAMDX(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.NodeMaxPayloadsAMDX(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.TrackFinishWritingAMDX -> stream.WriteUInt32(arg2.Value)
            | Decoration.PayloadNodeNameAMDX(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PayloadNodeBaseIndexAMDX(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PayloadNodeSparseArrayAMDX -> stream.WriteUInt32(arg2.Value)
            | Decoration.PayloadNodeArraySizeAMDX(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PayloadDispatchIndirectAMDX -> stream.WriteUInt32(arg2.Value)
            | Decoration.OverrideCoverageNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PassthroughNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.ViewportRelativeNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.SecondaryViewportRelativeNV(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PerPrimitiveEXT -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerViewNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerTaskNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerVertexKHR -> stream.WriteUInt32(arg2.Value)
            | Decoration.NonUniform -> stream.WriteUInt32(arg2.Value)
            | Decoration.RestrictPointer -> stream.WriteUInt32(arg2.Value)
            | Decoration.AliasedPointer -> stream.WriteUInt32(arg2.Value)
            | Decoration.HitObjectShaderRecordBufferNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.BindlessSamplerNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.BindlessImageNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.BoundSamplerNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.BoundImageNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.SIMTCallINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.ReferencedIndirectlyINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.ClobberINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
            | Decoration.SideEffectsINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.VectorComputeVariableINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.FuncParamIOKindINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.VectorComputeFunctionINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.StackCallINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.GlobalVariableOffsetINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.CounterBuffer(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.UserSemantic(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
            | Decoration.UserTypeGOOGLE(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
            | Decoration.FunctionRoundingModeINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteEnum(arg2_arg1)
            | Decoration.FunctionDenormModeINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteEnum(arg2_arg1)
            | Decoration.RegisterINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MemoryINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
            | Decoration.NumbanksINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.BankwidthINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MaxPrivateCopiesINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.SinglepumpINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.DoublepumpINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MaxReplicatesINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.SimpleDualPortINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MergeINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0);stream.WriteString(arg2_arg1)
            | Decoration.BankBitsINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.ForcePow2DepthINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.StridesizeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.WordsizeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.TrueDualPortINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.BurstCoalesceINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.CacheSizeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.DontStaticallyCoalesceINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.PrefetchINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.StallEnableINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.FuseLoopsInFunctionINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MathOpDSPModeINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteUInt32(arg2_arg1)
            | Decoration.AliasScopeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.NoAliasINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.InitiationIntervalINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MaxConcurrencyINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PipelineEnableINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.BufferLocationINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.IOPipeStorageINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.FunctionFloatingPointModeINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteEnum(arg2_arg1)
            | Decoration.SingleElementVectorINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.VectorComputeCallableFunctionINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MediaBlockIOINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.StallFreeINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.FPMaxErrorDecorationINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.LatencyControlLabelINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.LatencyControlConstraintINTEL(arg2_arg0, arg2_arg1, arg2_arg2) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteUInt32(arg2_arg1);stream.WriteUInt32(arg2_arg2)
            | Decoration.ConduitKernelArgumentINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.RegisterMapKernelArgumentINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MMHostInterfaceAddressWidthINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MMHostInterfaceDataWidthINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MMHostInterfaceLatencyINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MMHostInterfaceReadWriteModeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.MMHostInterfaceMaxBurstINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MMHostInterfaceWaitRequestINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.StableKernelArgumentINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.HostAccessINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0);stream.WriteString(arg2_arg1)
            | Decoration.InitModeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.ImplementInRegisterMapINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.CacheControlLoadINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteEnum(arg2_arg1)
            | Decoration.CacheControlStoreINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteEnum(arg2_arg1)
        | OpDecorationGroup(arg0) ->
            stream.WriteUInt32(arg0)
        | OpGroupDecorate(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteList(arg1, fun v -> stream.WriteUInt32(v))
        | OpGroupMemberDecorate(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteList(arg1, fun v -> match v with PairIdRefLiteralInteger(v_0, v_1) -> stream.WriteUInt32(v_0);stream.WriteUInt32(v_1))
        | OpVectorExtractDynamic(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpVectorInsertDynamic(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpVectorShuffle(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteList(arg4, fun v -> stream.WriteUInt32(v))
        | OpCompositeConstruct(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteList(arg2, fun v -> stream.WriteUInt32(v))
        | OpCompositeExtract(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpCompositeInsert(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteList(arg4, fun v -> stream.WriteUInt32(v))
        | OpCopyObject(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpTranspose(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSampledImage(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpImageSampleImplicitLod(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSampleExplicitLod(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            
            match arg4 with
            | ImageOperands.None -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Bias(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Lod(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Grad(arg4_arg0, arg4_arg1) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0);stream.WriteUInt32(arg4_arg1)
            | ImageOperands.ConstOffset(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Offset(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.ConstOffsets(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Sample(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MinLod(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MakeTexelAvailable(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MakeTexelVisible(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Offsets(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
        | OpImageSampleDrefImplicitLod(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSampleDrefExplicitLod(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            
            match arg5 with
            | ImageOperands.None -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Bias(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Lod(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Grad(arg5_arg0, arg5_arg1) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0);stream.WriteUInt32(arg5_arg1)
            | ImageOperands.ConstOffset(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Offset(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.ConstOffsets(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Sample(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MinLod(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MakeTexelAvailable(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MakeTexelVisible(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Offsets(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
        | OpImageSampleProjImplicitLod(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSampleProjExplicitLod(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            
            match arg4 with
            | ImageOperands.None -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Bias(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Lod(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Grad(arg4_arg0, arg4_arg1) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0);stream.WriteUInt32(arg4_arg1)
            | ImageOperands.ConstOffset(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Offset(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.ConstOffsets(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Sample(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MinLod(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MakeTexelAvailable(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MakeTexelVisible(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Offsets(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
        | OpImageSampleProjDrefImplicitLod(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSampleProjDrefExplicitLod(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            
            match arg5 with
            | ImageOperands.None -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Bias(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Lod(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Grad(arg5_arg0, arg5_arg1) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0);stream.WriteUInt32(arg5_arg1)
            | ImageOperands.ConstOffset(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Offset(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.ConstOffsets(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Sample(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MinLod(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MakeTexelAvailable(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MakeTexelVisible(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Offsets(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
        | OpImageFetch(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageGather(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageDrefGather(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageRead(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageWrite(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteOption(arg3, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImage(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpImageQueryFormat(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpImageQueryOrder(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpImageQuerySizeLod(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpImageQuerySize(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpImageQueryLod(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpImageQueryLevels(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpImageQuerySamples(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertFToU(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertFToS(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertSToF(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertUToF(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpUConvert(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSConvert(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpFConvert(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpQuantizeToF16(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertPtrToU(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSatConvertSToU(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSatConvertUToS(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertUToPtr(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpPtrCastToGeneric(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpGenericCastToPtr(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpGenericCastToPtrExplicit(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
        | OpBitcast(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSNegate(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpFNegate(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpIAdd(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFAdd(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpISub(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFSub(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpIMul(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFMul(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUDiv(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSDiv(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFDiv(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUMod(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSRem(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSMod(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFRem(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFMod(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpVectorTimesScalar(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpMatrixTimesScalar(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpVectorTimesMatrix(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpMatrixTimesVector(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpMatrixTimesMatrix(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpOuterProduct(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpDot(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpIAddCarry(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpISubBorrow(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUMulExtended(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSMulExtended(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpAny(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpAll(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpIsNan(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpIsInf(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpIsFinite(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpIsNormal(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSignBitSet(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpLessOrGreater(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpOrdered(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUnordered(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpLogicalEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpLogicalNotEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpLogicalOr(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpLogicalAnd(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpLogicalNot(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSelect(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpIEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpINotEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUGreaterThan(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSGreaterThan(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUGreaterThanEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSGreaterThanEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpULessThan(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSLessThan(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpULessThanEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSLessThanEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFOrdEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFUnordEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFOrdNotEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFUnordNotEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFOrdLessThan(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFUnordLessThan(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFOrdGreaterThan(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFUnordGreaterThan(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFOrdLessThanEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFUnordLessThanEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFOrdGreaterThanEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFUnordGreaterThanEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpShiftRightLogical(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpShiftRightArithmetic(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpShiftLeftLogical(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpBitwiseOr(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpBitwiseXor(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpBitwiseAnd(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpNot(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpBitFieldInsert(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpBitFieldSExtract(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpBitFieldUExtract(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpBitReverse(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpBitCount(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpDPdx(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpDPdy(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpFwidth(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpDPdxFine(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpDPdyFine(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpFwidthFine(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpDPdxCoarse(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpDPdyCoarse(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpFwidthCoarse(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpEmitVertex ->
            ()
        | OpEndPrimitive ->
            ()
        | OpEmitStreamVertex(arg0) ->
            stream.WriteUInt32(arg0)
        | OpEndStreamPrimitive(arg0) ->
            stream.WriteUInt32(arg0)
        | OpControlBarrier(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpMemoryBarrier(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpAtomicLoad(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpAtomicStore(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpAtomicExchange(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAtomicCompareExchange(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpAtomicCompareExchangeWeak(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpAtomicIIncrement(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpAtomicIDecrement(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpAtomicIAdd(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAtomicISub(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAtomicSMin(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAtomicUMin(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAtomicSMax(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAtomicUMax(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAtomicAnd(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAtomicOr(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAtomicXor(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpPhi(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteList(arg2, fun v -> match v with PairIdRefIdRef(v_0, v_1) -> stream.WriteUInt32(v_0);stream.WriteUInt32(v_1))
        | OpLoopMerge(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            
            match arg2 with
            | LoopControl.None -> stream.WriteUInt32(arg2.Value)
            | LoopControl.Unroll -> stream.WriteUInt32(arg2.Value)
            | LoopControl.DontUnroll -> stream.WriteUInt32(arg2.Value)
            | LoopControl.DependencyInfinite -> stream.WriteUInt32(arg2.Value)
            | LoopControl.DependencyLength(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.MinIterations(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.MaxIterations(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.IterationMultiple(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.PeelCount(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.PartialCount(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.InitiationIntervalINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.MaxConcurrencyINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.DependencyArrayINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.PipelineEnableINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.LoopCoalesceINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.MaxInterleavingINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.SpeculatedIterationsINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.NoFusionINTEL -> stream.WriteUInt32(arg2.Value)
            | LoopControl.LoopCountINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | LoopControl.MaxReinvocationDelayINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
        | OpSelectionMerge(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteEnum(arg1)
        | OpLabel(arg0) ->
            stream.WriteUInt32(arg0)
        | OpBranch(arg0) ->
            stream.WriteUInt32(arg0)
        | OpBranchConditional(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpSwitch(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteList(arg2, fun v -> match v with PairLiteralIntegerIdRef(v_0, v_1) -> stream.WriteUInt32(v_0);stream.WriteUInt32(v_1))
        | OpKill ->
            ()
        | OpReturn ->
            ()
        | OpReturnValue(arg0) ->
            stream.WriteUInt32(arg0)
        | OpUnreachable ->
            ()
        | OpLifetimeStart(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpLifetimeStop(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpGroupAsyncCopy(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpGroupWaitEvents(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpGroupAll(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGroupAny(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGroupBroadcast(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupIAdd(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupFAdd(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupFMin(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupUMin(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupSMin(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupFMax(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupUMax(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupSMax(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpReadPipe(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpWritePipe(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpReservedReadPipe(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpReservedWritePipe(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpReserveReadPipePackets(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpReserveWritePipePackets(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpCommitReadPipe(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpCommitWritePipe(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpIsValidReserveId(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpGetNumPipePackets(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGetMaxPipePackets(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupReserveReadPipePackets(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpGroupReserveWritePipePackets(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpGroupCommitReadPipe(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupCommitWritePipe(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpEnqueueMarker(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpEnqueueKernel(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
            stream.WriteUInt32(arg11)
            stream.WriteList(arg12, fun v -> stream.WriteUInt32(v))
        | OpGetKernelNDrangeSubGroupCount(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpGetKernelNDrangeMaxSubGroupSize(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpGetKernelWorkGroupSize(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpGetKernelPreferredWorkGroupSizeMultiple(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpRetainEvent(arg0) ->
            stream.WriteUInt32(arg0)
        | OpReleaseEvent(arg0) ->
            stream.WriteUInt32(arg0)
        | OpCreateUserEvent(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpIsValidEvent(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSetUserEventStatus(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpCaptureEventProfilingInfo(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpGetDefaultQueue(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpBuildNDRange(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpImageSparseSampleImplicitLod(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSparseSampleExplicitLod(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            
            match arg4 with
            | ImageOperands.None -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Bias(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Lod(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Grad(arg4_arg0, arg4_arg1) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0);stream.WriteUInt32(arg4_arg1)
            | ImageOperands.ConstOffset(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Offset(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.ConstOffsets(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Sample(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MinLod(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MakeTexelAvailable(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MakeTexelVisible(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Offsets(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
        | OpImageSparseSampleDrefImplicitLod(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSparseSampleDrefExplicitLod(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            
            match arg5 with
            | ImageOperands.None -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Bias(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Lod(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Grad(arg5_arg0, arg5_arg1) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0);stream.WriteUInt32(arg5_arg1)
            | ImageOperands.ConstOffset(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Offset(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.ConstOffsets(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Sample(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MinLod(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MakeTexelAvailable(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MakeTexelVisible(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Offsets(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
        | OpImageSparseSampleProjImplicitLod(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSparseSampleProjExplicitLod(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            
            match arg4 with
            | ImageOperands.None -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Bias(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Lod(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Grad(arg4_arg0, arg4_arg1) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0);stream.WriteUInt32(arg4_arg1)
            | ImageOperands.ConstOffset(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Offset(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.ConstOffsets(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.Sample(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MinLod(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MakeTexelAvailable(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.MakeTexelVisible(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(arg4.Value)
            | ImageOperands.Offsets(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
        | OpImageSparseSampleProjDrefImplicitLod(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSparseSampleProjDrefExplicitLod(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            
            match arg5 with
            | ImageOperands.None -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Bias(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Lod(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Grad(arg5_arg0, arg5_arg1) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0);stream.WriteUInt32(arg5_arg1)
            | ImageOperands.ConstOffset(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Offset(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.ConstOffsets(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.Sample(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MinLod(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MakeTexelAvailable(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.MakeTexelVisible(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(arg5.Value)
            | ImageOperands.Offsets(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
        | OpImageSparseFetch(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSparseGather(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSparseDrefGather(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpImageSparseTexelsResident(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpNoLine ->
            ()
        | OpAtomicFlagTestAndSet(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpAtomicFlagClear(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpImageSparseRead(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpSizeOf(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpTypePipeStorage(arg0) ->
            stream.WriteUInt32(arg0)
        | OpConstantPipeStorage(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpCreatePipeFromPipeStorage(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpGetKernelLocalSizeForSubgroupCount(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpGetKernelMaxNumSubgroups(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpTypeNamedBarrier(arg0) ->
            stream.WriteUInt32(arg0)
        | OpNamedBarrierInitialize(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpMemoryNamedBarrier(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpModuleProcessed(arg0) ->
            stream.WriteString(arg0)
        | OpExecutionModeId(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            
            match arg1 with
            | ExecutionMode.Invocations(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SpacingEqual -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SpacingFractionalEven -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SpacingFractionalOdd -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.VertexOrderCw -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.VertexOrderCcw -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PixelCenterInteger -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OriginUpperLeft -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OriginLowerLeft -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.EarlyFragmentTests -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PointMode -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Xfb -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DepthReplacing -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DepthGreater -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DepthLess -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DepthUnchanged -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.LocalSize(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.LocalSizeHint(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.InputPoints -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.InputLines -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.InputLinesAdjacency -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Triangles -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.InputTrianglesAdjacency -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Quads -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Isolines -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputVertices(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.OutputPoints -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputLineStrip -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputTriangleStrip -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.VecTypeHint(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.ContractionOff -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Initializer -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.Finalizer -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SubgroupSize(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SubgroupsPerWorkgroup(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SubgroupsPerWorkgroupId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.LocalSizeId(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.LocalSizeHintId(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.NonCoherentColorAttachmentReadEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.NonCoherentDepthAttachmentReadEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.NonCoherentStencilAttachmentReadEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SubgroupUniformControlFlowKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PostDepthCoverage -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DenormPreserve(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.DenormFlushToZero(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SignedZeroInfNanPreserve(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTE(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTZ(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.EarlyAndLateFragmentTestsAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefReplacingEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.CoalescingAMDX -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.IsApiEntryAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaxNodeRecursionAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.StaticNumWorkgroupsAMDX(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.ShaderIndexAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaxNumWorkgroupsAMDX(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.StencilRefUnchangedFrontAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefGreaterFrontAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefLessFrontAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefUnchangedBackAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefGreaterBackAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.StencilRefLessBackAMD -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.QuadDerivativesKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.RequireFullQuadsKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SharesInputWithAMDX(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1)
            | ExecutionMode.OutputLinesEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputPrimitivesEXT(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.DerivativeGroupQuadsKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DerivativeGroupLinearKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputTrianglesEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PixelInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PixelInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SampleInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SampleInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.ShadingRateInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.ShadingRateInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SharedLocalMemorySizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTPINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTNINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.FloatingPointModeALTINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.FloatingPointModeIEEEINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaxWorkgroupSizeINTEL(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | ExecutionMode.MaxWorkDimINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.NoGlobalOffsetINTEL -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.NumSIMDWorkitemsINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SchedulerTargetFmaxMhzINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaximallyReconvergesKHR -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.FPFastMathDefault(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1)
            | ExecutionMode.StreamingInterfaceINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RegisterMapInterfaceINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.NamedBarrierCountINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaximumRegistersINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.MaximumRegistersIdINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.NamedMaximumRegistersINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
        | OpDecorateId(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            
            match arg1 with
            | Decoration.RelaxedPrecision -> stream.WriteUInt32(arg1.Value)
            | Decoration.SpecId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Block -> stream.WriteUInt32(arg1.Value)
            | Decoration.BufferBlock -> stream.WriteUInt32(arg1.Value)
            | Decoration.RowMajor -> stream.WriteUInt32(arg1.Value)
            | Decoration.ColMajor -> stream.WriteUInt32(arg1.Value)
            | Decoration.ArrayStride(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MatrixStride(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.GLSLShared -> stream.WriteUInt32(arg1.Value)
            | Decoration.GLSLPacked -> stream.WriteUInt32(arg1.Value)
            | Decoration.CPacked -> stream.WriteUInt32(arg1.Value)
            | Decoration.BuiltIn(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.NoPerspective -> stream.WriteUInt32(arg1.Value)
            | Decoration.Flat -> stream.WriteUInt32(arg1.Value)
            | Decoration.Patch -> stream.WriteUInt32(arg1.Value)
            | Decoration.Centroid -> stream.WriteUInt32(arg1.Value)
            | Decoration.Sample -> stream.WriteUInt32(arg1.Value)
            | Decoration.Invariant -> stream.WriteUInt32(arg1.Value)
            | Decoration.Restrict -> stream.WriteUInt32(arg1.Value)
            | Decoration.Aliased -> stream.WriteUInt32(arg1.Value)
            | Decoration.Volatile -> stream.WriteUInt32(arg1.Value)
            | Decoration.Constant -> stream.WriteUInt32(arg1.Value)
            | Decoration.Coherent -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonWritable -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonReadable -> stream.WriteUInt32(arg1.Value)
            | Decoration.Uniform -> stream.WriteUInt32(arg1.Value)
            | Decoration.UniformId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.SaturatedConversion -> stream.WriteUInt32(arg1.Value)
            | Decoration.Stream(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Location(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Component(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Index(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Binding(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.DescriptorSet(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Offset(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.XfbBuffer(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.XfbStride(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.FuncParamAttr(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.FPRoundingMode(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.FPFastMathMode(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.LinkageAttributes(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.NoContraction -> stream.WriteUInt32(arg1.Value)
            | Decoration.InputAttachmentIndex(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Alignment(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxByteOffset(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.AlignmentId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxByteOffsetId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.NoSignedWrap -> stream.WriteUInt32(arg1.Value)
            | Decoration.NoUnsignedWrap -> stream.WriteUInt32(arg1.Value)
            | Decoration.WeightTextureQCOM -> stream.WriteUInt32(arg1.Value)
            | Decoration.BlockMatchTextureQCOM -> stream.WriteUInt32(arg1.Value)
            | Decoration.BlockMatchSamplerQCOM -> stream.WriteUInt32(arg1.Value)
            | Decoration.ExplicitInterpAMD -> stream.WriteUInt32(arg1.Value)
            | Decoration.NodeSharesPayloadLimitsWithAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.NodeMaxPayloadsAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.TrackFinishWritingAMDX -> stream.WriteUInt32(arg1.Value)
            | Decoration.PayloadNodeNameAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PayloadNodeBaseIndexAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PayloadNodeSparseArrayAMDX -> stream.WriteUInt32(arg1.Value)
            | Decoration.PayloadNodeArraySizeAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PayloadDispatchIndirectAMDX -> stream.WriteUInt32(arg1.Value)
            | Decoration.OverrideCoverageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PassthroughNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.ViewportRelativeNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.SecondaryViewportRelativeNV(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PerPrimitiveEXT -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerViewNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerTaskNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerVertexKHR -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonUniform -> stream.WriteUInt32(arg1.Value)
            | Decoration.RestrictPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.AliasedPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.HitObjectShaderRecordBufferNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BindlessSamplerNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BindlessImageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BoundSamplerNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BoundImageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.SIMTCallINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.ReferencedIndirectlyINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.ClobberINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.SideEffectsINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.VectorComputeVariableINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.FuncParamIOKindINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.VectorComputeFunctionINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.StackCallINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.GlobalVariableOffsetINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.CounterBuffer(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.UserSemantic(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.UserTypeGOOGLE(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.FunctionRoundingModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.FunctionDenormModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.RegisterINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MemoryINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.NumbanksINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.BankwidthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxPrivateCopiesINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.SinglepumpINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.DoublepumpINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MaxReplicatesINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.SimpleDualPortINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MergeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0);stream.WriteString(arg1_arg1)
            | Decoration.BankBitsINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.ForcePow2DepthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.StridesizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.WordsizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.TrueDualPortINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.BurstCoalesceINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.CacheSizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.DontStaticallyCoalesceINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.PrefetchINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.StallEnableINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.FuseLoopsInFunctionINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MathOpDSPModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1)
            | Decoration.AliasScopeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.NoAliasINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.InitiationIntervalINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxConcurrencyINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PipelineEnableINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.BufferLocationINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.IOPipeStorageINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.FunctionFloatingPointModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.SingleElementVectorINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.VectorComputeCallableFunctionINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MediaBlockIOINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.StallFreeINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.FPMaxErrorDecorationINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.LatencyControlLabelINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.LatencyControlConstraintINTEL(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | Decoration.ConduitKernelArgumentINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.RegisterMapKernelArgumentINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MMHostInterfaceAddressWidthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceDataWidthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceLatencyINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceReadWriteModeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.MMHostInterfaceMaxBurstINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceWaitRequestINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.StableKernelArgumentINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.HostAccessINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0);stream.WriteString(arg1_arg1)
            | Decoration.InitModeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.ImplementInRegisterMapINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.CacheControlLoadINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.CacheControlStoreINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
        | OpGroupNonUniformElect(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpGroupNonUniformAll(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGroupNonUniformAny(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGroupNonUniformAllEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGroupNonUniformBroadcast(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupNonUniformBroadcastFirst(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGroupNonUniformBallot(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGroupNonUniformInverseBallot(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGroupNonUniformBallotBitExtract(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupNonUniformBallotBitCount(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupNonUniformBallotFindLSB(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGroupNonUniformBallotFindMSB(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpGroupNonUniformShuffle(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupNonUniformShuffleXor(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupNonUniformShuffleUp(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupNonUniformShuffleDown(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupNonUniformIAdd(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformFAdd(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformIMul(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformFMul(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformSMin(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformUMin(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformFMin(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformSMax(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformUMax(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformFMax(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformBitwiseAnd(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformBitwiseOr(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformBitwiseXor(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformLogicalAnd(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformLogicalOr(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformLogicalXor(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpGroupNonUniformQuadBroadcast(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupNonUniformQuadSwap(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpCopyLogical(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpPtrEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpPtrNotEqual(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpPtrDiff(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpColorAttachmentReadEXT(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteOption(arg3, fun v -> stream.WriteUInt32(v))
        | OpDepthAttachmentReadEXT(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteOption(arg2, fun v -> stream.WriteUInt32(v))
        | OpStencilAttachmentReadEXT(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteOption(arg2, fun v -> stream.WriteUInt32(v))
        | OpTerminateInvocation ->
            ()
        | OpTypeUntypedPointerKHR(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteEnum(arg1)
        | OpUntypedVariableKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteEnum(arg2)
            stream.WriteOption(arg3, fun v -> stream.WriteUInt32(v))
            stream.WriteOption(arg4, fun v -> stream.WriteUInt32(v))
        | OpUntypedAccessChainKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteList(arg4, fun v -> stream.WriteUInt32(v))
        | OpUntypedInBoundsAccessChainKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteList(arg4, fun v -> stream.WriteUInt32(v))
        | OpSubgroupBallotKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupFirstInvocationKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpUntypedPtrAccessChainKHR(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteList(arg5, fun v -> stream.WriteUInt32(v))
        | OpUntypedInBoundsPtrAccessChainKHR(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteList(arg5, fun v -> stream.WriteUInt32(v))
        | OpUntypedArrayLengthKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpUntypedPrefetchKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteOption(arg2, fun v -> stream.WriteUInt32(v))
            stream.WriteOption(arg3, fun v -> stream.WriteUInt32(v))
            stream.WriteOption(arg4, fun v -> stream.WriteUInt32(v))
        | OpSubgroupAllKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAnyKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAllEqualKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpGroupNonUniformRotateKHR(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteUInt32(v))
        | OpSubgroupReadInvocationKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpExtInstWithForwardRefsKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteList(arg4, fun v -> stream.WriteUInt32(v))
        | OpTraceRayKHR(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
        | OpExecuteCallableKHR(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpConvertUToAccelerationStructureKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpIgnoreIntersectionKHR ->
            ()
        | OpTerminateRayKHR ->
            ()
        | OpSDot(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> stream.WriteEnum(v))
        | OpUDot(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> stream.WriteEnum(v))
        | OpSUDot(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> stream.WriteEnum(v))
        | OpSDotAccSat(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteEnum(v))
        | OpUDotAccSat(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteEnum(v))
        | OpSUDotAccSat(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteEnum(v))
        | OpTypeCooperativeMatrixKHR(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpCooperativeMatrixLoadKHR(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> stream.WriteUInt32(v))
            stream.WriteOption(arg5, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpCooperativeMatrixStoreKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteOption(arg3, fun v -> stream.WriteUInt32(v))
            stream.WriteOption(arg4, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpCooperativeMatrixMulAddKHR(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> stream.WriteEnum(v))
        | OpCooperativeMatrixLengthKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConstantCompositeReplicateEXT(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSpecConstantCompositeReplicateEXT(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpCompositeConstructReplicateEXT(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpTypeRayQueryKHR(arg0) ->
            stream.WriteUInt32(arg0)
        | OpRayQueryInitializeKHR(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpRayQueryTerminateKHR(arg0) ->
            stream.WriteUInt32(arg0)
        | OpRayQueryGenerateIntersectionKHR(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpRayQueryConfirmIntersectionKHR(arg0) ->
            stream.WriteUInt32(arg0)
        | OpRayQueryProceedKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpRayQueryGetIntersectionTypeKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpImageSampleWeightedQCOM(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpImageBoxFilterQCOM(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpImageBlockMatchSSDQCOM(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpImageBlockMatchSADQCOM(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpImageBlockMatchWindowSSDQCOM(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpImageBlockMatchWindowSADQCOM(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpImageBlockMatchGatherSSDQCOM(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpImageBlockMatchGatherSADQCOM(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpGroupIAddNonUniformAMD(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupFAddNonUniformAMD(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupFMinNonUniformAMD(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupUMinNonUniformAMD(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupSMinNonUniformAMD(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupFMaxNonUniformAMD(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupUMaxNonUniformAMD(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupSMaxNonUniformAMD(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpFragmentMaskFetchAMD(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFragmentFetchAMD(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpReadClockKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpAllocateNodePayloadsAMDX(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpEnqueueNodePayloadsAMDX(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeNodePayloadArrayAMDX(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpFinishWritingNodePayloadAMDX(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpNodePayloadArrayLengthAMDX(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpIsNodePayloadValidAMDX(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpConstantStringAMDX(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteString(arg1)
        | OpSpecConstantStringAMDX(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteString(arg1)
        | OpGroupNonUniformQuadAllKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpGroupNonUniformQuadAnyKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectRecordHitMotionNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
            stream.WriteUInt32(arg11)
            stream.WriteUInt32(arg12)
            stream.WriteUInt32(arg13)
        | OpHitObjectRecordHitWithIndexMotionNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
            stream.WriteUInt32(arg11)
            stream.WriteUInt32(arg12)
        | OpHitObjectRecordMissMotionNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpHitObjectGetWorldToObjectNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetObjectToWorldNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetObjectRayDirectionNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetObjectRayOriginNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectTraceRayMotionNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
            stream.WriteUInt32(arg11)
            stream.WriteUInt32(arg12)
        | OpHitObjectGetShaderRecordBufferHandleNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetShaderBindingTableRecordIndexNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectRecordEmptyNV(arg0) ->
            stream.WriteUInt32(arg0)
        | OpHitObjectTraceRayNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
            stream.WriteUInt32(arg11)
        | OpHitObjectRecordHitNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
            stream.WriteUInt32(arg11)
            stream.WriteUInt32(arg12)
        | OpHitObjectRecordHitWithIndexNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
            stream.WriteUInt32(arg11)
        | OpHitObjectRecordMissNV(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpHitObjectExecuteShaderNV(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpHitObjectGetCurrentTimeNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetAttributesNV(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpHitObjectGetHitKindNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetPrimitiveIndexNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetGeometryIndexNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetInstanceIdNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetInstanceCustomIndexNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetWorldRayDirectionNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetWorldRayOriginNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetRayTMaxNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectGetRayTMinNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectIsEmptyNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectIsHitNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpHitObjectIsMissNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpReorderThreadWithHitObjectNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteOption(arg1, fun v -> stream.WriteUInt32(v))
            stream.WriteOption(arg2, fun v -> stream.WriteUInt32(v))
        | OpReorderThreadWithHintNV(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpTypeHitObjectNV(arg0) ->
            stream.WriteUInt32(arg0)
        | OpImageSampleFootprintNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteOption(arg6, fun v -> 
            match v with
            | ImageOperands.None -> stream.WriteUInt32(v.Value)
            | ImageOperands.Bias(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Lod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Grad(v_arg0, v_arg1) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0);stream.WriteUInt32(v_arg1)
            | ImageOperands.ConstOffset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Offset(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.ConstOffsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.Sample(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MinLod(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.MakeTexelVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | ImageOperands.NonPrivateTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.VolatileTexel -> stream.WriteUInt32(v.Value)
            | ImageOperands.SignExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value)
            | ImageOperands.Nontemporal -> stream.WriteUInt32(v.Value)
            | ImageOperands.Offsets(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpCooperativeMatrixConvertNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpEmitMeshTasksEXT(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteOption(arg3, fun v -> stream.WriteUInt32(v))
        | OpSetMeshOutputsEXT(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpGroupNonUniformPartitionNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpWritePackedPrimitiveIndices4x8NV(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpFetchMicroTriangleVertexPositionNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpFetchMicroTriangleVertexBarycentricNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpReportIntersectionKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpIgnoreIntersectionNV ->
            ()
        | OpTerminateRayNV ->
            ()
        | OpTraceNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
        | OpTraceMotionNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
            stream.WriteUInt32(arg11)
        | OpTraceRayMotionNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
            stream.WriteUInt32(arg11)
        | OpRayQueryGetIntersectionTriangleVertexPositionsKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpTypeAccelerationStructureKHR(arg0) ->
            stream.WriteUInt32(arg0)
        | OpExecuteCallableNV(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpTypeCooperativeMatrixNV(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpCooperativeMatrixLoadNV(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteOption(arg5, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpCooperativeMatrixStoreNV(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteOption(arg4, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpCooperativeMatrixMulAddNV(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpCooperativeMatrixLengthNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpBeginInvocationInterlockEXT ->
            ()
        | OpEndInvocationInterlockEXT ->
            ()
        | OpCooperativeMatrixReduceNV(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpCooperativeMatrixLoadTensorNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            
            match arg5 with
            | MemoryAccess.None -> stream.WriteUInt32(arg5.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(arg5.Value)
            | MemoryAccess.Aligned(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(arg5.Value)
            | MemoryAccess.MakePointerAvailable(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | MemoryAccess.MakePointerVisible(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(arg5.Value)
            | MemoryAccess.AliasScopeINTELMask(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            | MemoryAccess.NoAliasINTELMask(arg5_arg0) -> stream.WriteUInt32(arg5.Value);stream.WriteUInt32(arg5_arg0)
            
            match arg6 with
            | TensorAddressingOperands.None -> stream.WriteUInt32(arg6.Value)
            | TensorAddressingOperands.TensorView(arg6_arg0) -> stream.WriteUInt32(arg6.Value);stream.WriteUInt32(arg6_arg0)
            | TensorAddressingOperands.DecodeFunc(arg6_arg0) -> stream.WriteUInt32(arg6.Value);stream.WriteUInt32(arg6_arg0)
        | OpCooperativeMatrixStoreTensorNV(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            
            match arg3 with
            | MemoryAccess.None -> stream.WriteUInt32(arg3.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(arg3.Value)
            | MemoryAccess.Aligned(arg3_arg0) -> stream.WriteUInt32(arg3.Value);stream.WriteUInt32(arg3_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(arg3.Value)
            | MemoryAccess.MakePointerAvailable(arg3_arg0) -> stream.WriteUInt32(arg3.Value);stream.WriteUInt32(arg3_arg0)
            | MemoryAccess.MakePointerVisible(arg3_arg0) -> stream.WriteUInt32(arg3.Value);stream.WriteUInt32(arg3_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(arg3.Value)
            | MemoryAccess.AliasScopeINTELMask(arg3_arg0) -> stream.WriteUInt32(arg3.Value);stream.WriteUInt32(arg3_arg0)
            | MemoryAccess.NoAliasINTELMask(arg3_arg0) -> stream.WriteUInt32(arg3.Value);stream.WriteUInt32(arg3_arg0)
            
            match arg4 with
            | TensorAddressingOperands.None -> stream.WriteUInt32(arg4.Value)
            | TensorAddressingOperands.TensorView(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
            | TensorAddressingOperands.DecodeFunc(arg4_arg0) -> stream.WriteUInt32(arg4.Value);stream.WriteUInt32(arg4_arg0)
        | OpCooperativeMatrixPerElementOpNV(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteList(arg4, fun v -> stream.WriteUInt32(v))
        | OpTypeTensorLayoutNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpTypeTensorViewNV(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpCreateTensorLayoutNV(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpTensorLayoutSetDimensionNV(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpTensorLayoutSetStrideNV(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpTensorLayoutSliceNV(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpTensorLayoutSetClampValueNV(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpCreateTensorViewNV(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpTensorViewSetDimensionNV(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpTensorViewSetStrideNV(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpDemoteToHelperInvocation ->
            ()
        | OpIsHelperInvocationEXT(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpTensorViewSetClipNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpTensorLayoutSetBlockSizeNV(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpCooperativeMatrixTransposeNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertUToImageNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertUToSamplerNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertImageToUNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertSamplerToUNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertUToSampledImageNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertSampledImageToUNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSamplerImageAddressingModeNV(arg0) ->
            stream.WriteUInt32(arg0)
        | OpRawAccessChainNV(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteOption(arg6, fun v -> stream.WriteEnum(v))
        | OpSubgroupShuffleINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupShuffleDownINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupShuffleUpINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupShuffleXorINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupBlockReadINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupBlockWriteINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpSubgroupImageBlockReadINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupImageBlockWriteINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupImageMediaBlockReadINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupImageMediaBlockWriteINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpUCountLeadingZerosINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpUCountTrailingZerosINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpAbsISubINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpAbsUSubINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpIAddSatINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUAddSatINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpIAverageINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUAverageINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpIAverageRoundedINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUAverageRoundedINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpISubSatINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUSubSatINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpIMul32x16INTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpUMul32x16INTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpConstantFunctionPointerINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpFunctionPointerCallINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteList(arg2, fun v -> stream.WriteUInt32(v))
        | OpAsmTargetINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteString(arg2)
        | OpAsmINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteString(arg4)
            stream.WriteString(arg5)
        | OpAsmCallINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteList(arg3, fun v -> stream.WriteUInt32(v))
        | OpAtomicFMinEXT(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAtomicFMaxEXT(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpAssumeTrueKHR(arg0) ->
            stream.WriteUInt32(arg0)
        | OpExpectKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpDecorateString(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            
            match arg1 with
            | Decoration.RelaxedPrecision -> stream.WriteUInt32(arg1.Value)
            | Decoration.SpecId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Block -> stream.WriteUInt32(arg1.Value)
            | Decoration.BufferBlock -> stream.WriteUInt32(arg1.Value)
            | Decoration.RowMajor -> stream.WriteUInt32(arg1.Value)
            | Decoration.ColMajor -> stream.WriteUInt32(arg1.Value)
            | Decoration.ArrayStride(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MatrixStride(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.GLSLShared -> stream.WriteUInt32(arg1.Value)
            | Decoration.GLSLPacked -> stream.WriteUInt32(arg1.Value)
            | Decoration.CPacked -> stream.WriteUInt32(arg1.Value)
            | Decoration.BuiltIn(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.NoPerspective -> stream.WriteUInt32(arg1.Value)
            | Decoration.Flat -> stream.WriteUInt32(arg1.Value)
            | Decoration.Patch -> stream.WriteUInt32(arg1.Value)
            | Decoration.Centroid -> stream.WriteUInt32(arg1.Value)
            | Decoration.Sample -> stream.WriteUInt32(arg1.Value)
            | Decoration.Invariant -> stream.WriteUInt32(arg1.Value)
            | Decoration.Restrict -> stream.WriteUInt32(arg1.Value)
            | Decoration.Aliased -> stream.WriteUInt32(arg1.Value)
            | Decoration.Volatile -> stream.WriteUInt32(arg1.Value)
            | Decoration.Constant -> stream.WriteUInt32(arg1.Value)
            | Decoration.Coherent -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonWritable -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonReadable -> stream.WriteUInt32(arg1.Value)
            | Decoration.Uniform -> stream.WriteUInt32(arg1.Value)
            | Decoration.UniformId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.SaturatedConversion -> stream.WriteUInt32(arg1.Value)
            | Decoration.Stream(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Location(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Component(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Index(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Binding(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.DescriptorSet(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Offset(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.XfbBuffer(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.XfbStride(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.FuncParamAttr(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.FPRoundingMode(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.FPFastMathMode(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.LinkageAttributes(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.NoContraction -> stream.WriteUInt32(arg1.Value)
            | Decoration.InputAttachmentIndex(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.Alignment(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxByteOffset(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.AlignmentId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxByteOffsetId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.NoSignedWrap -> stream.WriteUInt32(arg1.Value)
            | Decoration.NoUnsignedWrap -> stream.WriteUInt32(arg1.Value)
            | Decoration.WeightTextureQCOM -> stream.WriteUInt32(arg1.Value)
            | Decoration.BlockMatchTextureQCOM -> stream.WriteUInt32(arg1.Value)
            | Decoration.BlockMatchSamplerQCOM -> stream.WriteUInt32(arg1.Value)
            | Decoration.ExplicitInterpAMD -> stream.WriteUInt32(arg1.Value)
            | Decoration.NodeSharesPayloadLimitsWithAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.NodeMaxPayloadsAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.TrackFinishWritingAMDX -> stream.WriteUInt32(arg1.Value)
            | Decoration.PayloadNodeNameAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PayloadNodeBaseIndexAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PayloadNodeSparseArrayAMDX -> stream.WriteUInt32(arg1.Value)
            | Decoration.PayloadNodeArraySizeAMDX(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PayloadDispatchIndirectAMDX -> stream.WriteUInt32(arg1.Value)
            | Decoration.OverrideCoverageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PassthroughNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.ViewportRelativeNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.SecondaryViewportRelativeNV(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PerPrimitiveEXT -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerViewNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerTaskNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerVertexKHR -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonUniform -> stream.WriteUInt32(arg1.Value)
            | Decoration.RestrictPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.AliasedPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.HitObjectShaderRecordBufferNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BindlessSamplerNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BindlessImageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BoundSamplerNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.BoundImageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.SIMTCallINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.ReferencedIndirectlyINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.ClobberINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.SideEffectsINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.VectorComputeVariableINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.FuncParamIOKindINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.VectorComputeFunctionINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.StackCallINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.GlobalVariableOffsetINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.CounterBuffer(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.UserSemantic(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.UserTypeGOOGLE(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.FunctionRoundingModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.FunctionDenormModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.RegisterINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MemoryINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.NumbanksINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.BankwidthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxPrivateCopiesINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.SinglepumpINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.DoublepumpINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MaxReplicatesINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.SimpleDualPortINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MergeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0);stream.WriteString(arg1_arg1)
            | Decoration.BankBitsINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.ForcePow2DepthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.StridesizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.WordsizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.TrueDualPortINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.BurstCoalesceINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.CacheSizeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.DontStaticallyCoalesceINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.PrefetchINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.StallEnableINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.FuseLoopsInFunctionINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MathOpDSPModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1)
            | Decoration.AliasScopeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.NoAliasINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.InitiationIntervalINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MaxConcurrencyINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PipelineEnableINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.BufferLocationINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.IOPipeStorageINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.FunctionFloatingPointModeINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.SingleElementVectorINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.VectorComputeCallableFunctionINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MediaBlockIOINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.StallFreeINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.FPMaxErrorDecorationINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.LatencyControlLabelINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.LatencyControlConstraintINTEL(arg1_arg0, arg1_arg1, arg1_arg2) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteUInt32(arg1_arg1);stream.WriteUInt32(arg1_arg2)
            | Decoration.ConduitKernelArgumentINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.RegisterMapKernelArgumentINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.MMHostInterfaceAddressWidthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceDataWidthINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceLatencyINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceReadWriteModeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.MMHostInterfaceMaxBurstINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.MMHostInterfaceWaitRequestINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.StableKernelArgumentINTEL -> stream.WriteUInt32(arg1.Value)
            | Decoration.HostAccessINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0);stream.WriteString(arg1_arg1)
            | Decoration.InitModeINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteEnum(arg1_arg0)
            | Decoration.ImplementInRegisterMapINTEL(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.CacheControlLoadINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
            | Decoration.CacheControlStoreINTEL(arg1_arg0, arg1_arg1) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0);stream.WriteEnum(arg1_arg1)
        | OpMemberDecorateString(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            
            match arg2 with
            | Decoration.RelaxedPrecision -> stream.WriteUInt32(arg2.Value)
            | Decoration.SpecId(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Block -> stream.WriteUInt32(arg2.Value)
            | Decoration.BufferBlock -> stream.WriteUInt32(arg2.Value)
            | Decoration.RowMajor -> stream.WriteUInt32(arg2.Value)
            | Decoration.ColMajor -> stream.WriteUInt32(arg2.Value)
            | Decoration.ArrayStride(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MatrixStride(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.GLSLShared -> stream.WriteUInt32(arg2.Value)
            | Decoration.GLSLPacked -> stream.WriteUInt32(arg2.Value)
            | Decoration.CPacked -> stream.WriteUInt32(arg2.Value)
            | Decoration.BuiltIn(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.NoPerspective -> stream.WriteUInt32(arg2.Value)
            | Decoration.Flat -> stream.WriteUInt32(arg2.Value)
            | Decoration.Patch -> stream.WriteUInt32(arg2.Value)
            | Decoration.Centroid -> stream.WriteUInt32(arg2.Value)
            | Decoration.Sample -> stream.WriteUInt32(arg2.Value)
            | Decoration.Invariant -> stream.WriteUInt32(arg2.Value)
            | Decoration.Restrict -> stream.WriteUInt32(arg2.Value)
            | Decoration.Aliased -> stream.WriteUInt32(arg2.Value)
            | Decoration.Volatile -> stream.WriteUInt32(arg2.Value)
            | Decoration.Constant -> stream.WriteUInt32(arg2.Value)
            | Decoration.Coherent -> stream.WriteUInt32(arg2.Value)
            | Decoration.NonWritable -> stream.WriteUInt32(arg2.Value)
            | Decoration.NonReadable -> stream.WriteUInt32(arg2.Value)
            | Decoration.Uniform -> stream.WriteUInt32(arg2.Value)
            | Decoration.UniformId(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.SaturatedConversion -> stream.WriteUInt32(arg2.Value)
            | Decoration.Stream(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Location(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Component(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Index(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Binding(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.DescriptorSet(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Offset(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.XfbBuffer(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.XfbStride(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.FuncParamAttr(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.FPRoundingMode(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.FPFastMathMode(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.LinkageAttributes(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0);stream.WriteEnum(arg2_arg1)
            | Decoration.NoContraction -> stream.WriteUInt32(arg2.Value)
            | Decoration.InputAttachmentIndex(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.Alignment(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MaxByteOffset(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.AlignmentId(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MaxByteOffsetId(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.NoSignedWrap -> stream.WriteUInt32(arg2.Value)
            | Decoration.NoUnsignedWrap -> stream.WriteUInt32(arg2.Value)
            | Decoration.WeightTextureQCOM -> stream.WriteUInt32(arg2.Value)
            | Decoration.BlockMatchTextureQCOM -> stream.WriteUInt32(arg2.Value)
            | Decoration.BlockMatchSamplerQCOM -> stream.WriteUInt32(arg2.Value)
            | Decoration.ExplicitInterpAMD -> stream.WriteUInt32(arg2.Value)
            | Decoration.NodeSharesPayloadLimitsWithAMDX(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.NodeMaxPayloadsAMDX(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.TrackFinishWritingAMDX -> stream.WriteUInt32(arg2.Value)
            | Decoration.PayloadNodeNameAMDX(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PayloadNodeBaseIndexAMDX(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PayloadNodeSparseArrayAMDX -> stream.WriteUInt32(arg2.Value)
            | Decoration.PayloadNodeArraySizeAMDX(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PayloadDispatchIndirectAMDX -> stream.WriteUInt32(arg2.Value)
            | Decoration.OverrideCoverageNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PassthroughNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.ViewportRelativeNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.SecondaryViewportRelativeNV(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PerPrimitiveEXT -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerViewNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerTaskNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerVertexKHR -> stream.WriteUInt32(arg2.Value)
            | Decoration.NonUniform -> stream.WriteUInt32(arg2.Value)
            | Decoration.RestrictPointer -> stream.WriteUInt32(arg2.Value)
            | Decoration.AliasedPointer -> stream.WriteUInt32(arg2.Value)
            | Decoration.HitObjectShaderRecordBufferNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.BindlessSamplerNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.BindlessImageNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.BoundSamplerNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.BoundImageNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.SIMTCallINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.ReferencedIndirectlyINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.ClobberINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
            | Decoration.SideEffectsINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.VectorComputeVariableINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.FuncParamIOKindINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.VectorComputeFunctionINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.StackCallINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.GlobalVariableOffsetINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.CounterBuffer(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.UserSemantic(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
            | Decoration.UserTypeGOOGLE(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
            | Decoration.FunctionRoundingModeINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteEnum(arg2_arg1)
            | Decoration.FunctionDenormModeINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteEnum(arg2_arg1)
            | Decoration.RegisterINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MemoryINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
            | Decoration.NumbanksINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.BankwidthINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MaxPrivateCopiesINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.SinglepumpINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.DoublepumpINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MaxReplicatesINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.SimpleDualPortINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MergeINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0);stream.WriteString(arg2_arg1)
            | Decoration.BankBitsINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.ForcePow2DepthINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.StridesizeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.WordsizeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.TrueDualPortINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.BurstCoalesceINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.CacheSizeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.DontStaticallyCoalesceINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.PrefetchINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.StallEnableINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.FuseLoopsInFunctionINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MathOpDSPModeINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteUInt32(arg2_arg1)
            | Decoration.AliasScopeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.NoAliasINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.InitiationIntervalINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MaxConcurrencyINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PipelineEnableINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.BufferLocationINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.IOPipeStorageINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.FunctionFloatingPointModeINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteEnum(arg2_arg1)
            | Decoration.SingleElementVectorINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.VectorComputeCallableFunctionINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MediaBlockIOINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.StallFreeINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.FPMaxErrorDecorationINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.LatencyControlLabelINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.LatencyControlConstraintINTEL(arg2_arg0, arg2_arg1, arg2_arg2) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteUInt32(arg2_arg1);stream.WriteUInt32(arg2_arg2)
            | Decoration.ConduitKernelArgumentINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.RegisterMapKernelArgumentINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.MMHostInterfaceAddressWidthINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MMHostInterfaceDataWidthINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MMHostInterfaceLatencyINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MMHostInterfaceReadWriteModeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.MMHostInterfaceMaxBurstINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.MMHostInterfaceWaitRequestINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.StableKernelArgumentINTEL -> stream.WriteUInt32(arg2.Value)
            | Decoration.HostAccessINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0);stream.WriteString(arg2_arg1)
            | Decoration.InitModeINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteEnum(arg2_arg0)
            | Decoration.ImplementInRegisterMapINTEL(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.CacheControlLoadINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteEnum(arg2_arg1)
            | Decoration.CacheControlStoreINTEL(arg2_arg0, arg2_arg1) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0);stream.WriteEnum(arg2_arg1)
        | OpVmeImageINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpTypeVmeImageINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpTypeAvcImePayloadINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcRefPayloadINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcSicPayloadINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcMcePayloadINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcMceResultINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcImeResultINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcImeResultSingleReferenceStreamoutINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcImeResultDualReferenceStreamoutINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcImeSingleReferenceStreaminINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcImeDualReferenceStreaminINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcRefResultINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpTypeAvcSicResultINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpSubgroupAvcMceGetDefaultInterBaseMultiReferencePenaltyINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceSetInterBaseMultiReferencePenaltyINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceGetDefaultInterShapePenaltyINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceSetInterShapePenaltyINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceGetDefaultInterDirectionPenaltyINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceSetInterDirectionPenaltyINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceGetDefaultIntraLumaShapePenaltyINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceGetDefaultInterMotionVectorCostTableINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceGetDefaultHighPenaltyCostTableINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpSubgroupAvcMceGetDefaultMediumPenaltyCostTableINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpSubgroupAvcMceGetDefaultLowPenaltyCostTableINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpSubgroupAvcMceSetMotionVectorCostFunctionINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcMceGetDefaultIntraLumaModePenaltyINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceGetDefaultNonDcLumaIntraPenaltyINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpSubgroupAvcMceGetDefaultIntraChromaModeBasePenaltyINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpSubgroupAvcMceSetAcOnlyHaarINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceSetSourceInterlacedFieldPolarityINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceSetSingleReferenceInterlacedFieldPolarityINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcMceSetDualReferenceInterlacedFieldPolaritiesINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcMceConvertToImePayloadINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceConvertToImeResultINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceConvertToRefPayloadINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceConvertToRefResultINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceConvertToSicPayloadINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceConvertToSicResultINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceGetMotionVectorsINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceGetInterDistortionsINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceGetBestInterDistortionsINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceGetInterMajorShapeINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceGetInterMinorShapeINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceGetInterDirectionsINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceGetInterMotionVectorCountINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceGetInterReferenceIdsINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcMceGetInterReferenceInterlacedFieldPolaritiesINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcImeInitializeINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcImeSetSingleReferenceINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcImeSetDualReferenceINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcImeRefWindowSizeINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcImeAdjustRefOffsetINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcImeConvertToMcePayloadINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcImeSetMaxMotionVectorCountINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcImeSetUnidirectionalMixDisableINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcImeSetEarlySearchTerminationThresholdINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcImeSetWeightedSadINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcImeEvaluateWithSingleReferenceINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcImeEvaluateWithDualReferenceINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcImeEvaluateWithDualReferenceStreaminINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpSubgroupAvcImeEvaluateWithSingleReferenceStreamoutINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcImeEvaluateWithDualReferenceStreamoutINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminoutINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcImeEvaluateWithDualReferenceStreaminoutINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpSubgroupAvcImeConvertToMceResultINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcImeGetSingleReferenceStreaminINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcImeGetDualReferenceStreaminINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcImeStripSingleReferenceStreamoutINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcImeStripDualReferenceStreamoutINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeMotionVectorsINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeDistortionsINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeReferenceIdsINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeMotionVectorsINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeDistortionsINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeReferenceIdsINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcImeGetBorderReachedINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcImeGetTruncatedSearchIndicationINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcImeGetUnidirectionalEarlySearchTerminationINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcImeGetWeightingPatternMinimumMotionVectorINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcImeGetWeightingPatternMinimumDistortionINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcFmeInitializeINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpSubgroupAvcBmeInitializeINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
        | OpSubgroupAvcRefConvertToMcePayloadINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcRefSetBidirectionalMixDisableINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcRefSetBilinearFilterEnableINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcRefEvaluateWithSingleReferenceINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcRefEvaluateWithDualReferenceINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcRefEvaluateWithMultiReferenceINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcRefEvaluateWithMultiReferenceInterlacedINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcRefConvertToMceResultINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicInitializeINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicConfigureSkcINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpSubgroupAvcSicConfigureIpeLumaINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
        | OpSubgroupAvcSicConfigureIpeLumaChromaINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
            stream.WriteUInt32(arg10)
            stream.WriteUInt32(arg11)
            stream.WriteUInt32(arg12)
        | OpSubgroupAvcSicGetMotionVectorMaskINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcSicConvertToMcePayloadINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicSetIntraLumaShapePenaltyINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcSicSetIntraLumaModeCostFunctionINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcSicSetIntraChromaModeCostFunctionINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcSicSetBilinearFilterEnableINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicSetSkcForwardTransformEnableINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcSicSetBlockBasedRawSkipSadINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcSicEvaluateIpeINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpSubgroupAvcSicEvaluateWithSingleReferenceINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcSicEvaluateWithDualReferenceINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcSicEvaluateWithMultiReferenceINTEL(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
        | OpSubgroupAvcSicEvaluateWithMultiReferenceInterlacedINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpSubgroupAvcSicConvertToMceResultINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicGetIpeLumaShapeINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicGetBestIpeLumaDistortionINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicGetBestIpeChromaDistortionINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicGetPackedIpeLumaModesINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicGetIpeChromaModeINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicGetPackedSkcLumaCountThresholdINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicGetPackedSkcLumaSumThresholdINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupAvcSicGetInterRawSadsINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpVariableLengthArrayINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSaveMemoryINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpRestoreMemoryINTEL(arg0) ->
            stream.WriteUInt32(arg0)
        | OpArbitraryFloatSinCosPiINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpArbitraryFloatCastINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatCastFromIntINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatCastToIntINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
        | OpArbitraryFloatAddINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
        | OpArbitraryFloatSubINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
        | OpArbitraryFloatMulINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
        | OpArbitraryFloatDivINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
        | OpArbitraryFloatGTINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpArbitraryFloatGEINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpArbitraryFloatLTINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpArbitraryFloatLEINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpArbitraryFloatEQINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpArbitraryFloatRecipINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatRSqrtINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatCbrtINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatHypotINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
        | OpArbitraryFloatSqrtINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatLogINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatLog2INTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatLog10INTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatLog1pINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatExpINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatExp2INTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatExp10INTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatExpm1INTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatSinINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatCosINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatSinCosINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatSinPiINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatCosPiINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatASinINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatASinPiINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatACosINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatACosPiINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatATanINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatATanPiINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
        | OpArbitraryFloatATan2INTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
        | OpArbitraryFloatPowINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
        | OpArbitraryFloatPowRINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
            stream.WriteUInt32(arg9)
        | OpArbitraryFloatPowNINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpLoopControlINTEL(arg0) ->
            stream.WriteList(arg0, fun v -> stream.WriteUInt32(v))
        | OpAliasDomainDeclINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteOption(arg1, fun v -> stream.WriteUInt32(v))
        | OpAliasScopeDeclINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteOption(arg2, fun v -> stream.WriteUInt32(v))
        | OpAliasScopeListDeclINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteList(arg1, fun v -> stream.WriteUInt32(v))
        | OpFixedSqrtINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpFixedRecipINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpFixedRsqrtINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpFixedSinINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpFixedCosINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpFixedSinCosINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpFixedSinPiINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpFixedCosPiINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpFixedSinCosPiINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpFixedLogINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpFixedExpINTEL(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
            stream.WriteUInt32(arg6)
            stream.WriteUInt32(arg7)
            stream.WriteUInt32(arg8)
        | OpPtrCastToCrossWorkgroupINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpCrossWorkgroupCastToPtrINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpReadPipeBlockingINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpWritePipeBlockingINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpFPGARegINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetRayTMinKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpRayQueryGetRayFlagsKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpRayQueryGetIntersectionTKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetIntersectionInstanceCustomIndexKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetIntersectionInstanceIdKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetIntersectionGeometryIndexKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetIntersectionPrimitiveIndexKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetIntersectionBarycentricsKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetIntersectionFrontFaceKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetIntersectionCandidateAABBOpaqueKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpRayQueryGetIntersectionObjectRayDirectionKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetIntersectionObjectRayOriginKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetWorldRayDirectionKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpRayQueryGetWorldRayOriginKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpRayQueryGetIntersectionObjectToWorldKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpRayQueryGetIntersectionWorldToObjectKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
        | OpAtomicFAddEXT(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpTypeBufferSurfaceINTEL(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteEnum(arg1)
        | OpTypeStructContinuedINTEL(arg0) ->
            stream.WriteList(arg0, fun v -> stream.WriteUInt32(v))
        | OpConstantCompositeContinuedINTEL(arg0) ->
            stream.WriteList(arg0, fun v -> stream.WriteUInt32(v))
        | OpSpecConstantCompositeContinuedINTEL(arg0) ->
            stream.WriteList(arg0, fun v -> stream.WriteUInt32(v))
        | OpCompositeConstructContinuedINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteList(arg2, fun v -> stream.WriteUInt32(v))
        | OpConvertFToBF16INTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpConvertBF16ToFINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpControlBarrierArriveINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpControlBarrierWaitINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpArithmeticFenceEXT(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupBlockPrefetchINTEL(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteOption(arg2, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value)
            | MemoryAccess.AliasScopeINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NoAliasINTELMask(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0))
        | OpGroupIMulKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupFMulKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupBitwiseAndKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupBitwiseOrKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupBitwiseXorKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupLogicalAndKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupLogicalOrKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpGroupLogicalXorKHR(arg0, arg1, arg2, arg3, arg4) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteEnum(arg3)
            stream.WriteUInt32(arg4)
        | OpMaskedGatherINTEL(arg0, arg1, arg2, arg3, arg4, arg5) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
            stream.WriteUInt32(arg4)
            stream.WriteUInt32(arg5)
        | OpMaskedScatterINTEL(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)

    static member internal Deserialize(opcode: uint16, stream: SpirvStream) =
        match opcode with
        | 0us ->
            OpNop
        | 1us ->
            OpUndef(stream.ReadUInt32(), stream.ReadUInt32())
        | 2us ->
            OpSourceContinued(stream.ReadString())
        | 3us ->
            OpSource(stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()), stream.ReadOption(fun () -> stream.ReadString()))
        | 4us ->
            OpSourceExtension(stream.ReadString())
        | 5us ->
            OpName(stream.ReadUInt32(), stream.ReadString())
        | 6us ->
            OpMemberName(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadString())
        | 7us ->
            OpString(stream.ReadUInt32(), stream.ReadString())
        | 8us ->
            OpLine(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 10us ->
            OpExtension(stream.ReadString())
        | 11us ->
            OpExtInstImport(stream.ReadUInt32(), stream.ReadString())
        | 12us ->
            OpExtInst(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 14us ->
            OpMemoryModel(stream.ReadEnum(), stream.ReadEnum())
        | 15us ->
            OpEntryPoint(stream.ReadEnum(), stream.ReadUInt32(), stream.ReadString(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 16us ->
            OpExecutionMode(stream.ReadUInt32(), (match stream.ReadUInt32() with | 0u -> ExecutionMode.Invocations(stream.ReadUInt32()) | 1u -> ExecutionMode.SpacingEqual | 2u -> ExecutionMode.SpacingFractionalEven | 3u -> ExecutionMode.SpacingFractionalOdd | 4u -> ExecutionMode.VertexOrderCw | 5u -> ExecutionMode.VertexOrderCcw | 6u -> ExecutionMode.PixelCenterInteger | 7u -> ExecutionMode.OriginUpperLeft | 8u -> ExecutionMode.OriginLowerLeft | 9u -> ExecutionMode.EarlyFragmentTests | 10u -> ExecutionMode.PointMode | 11u -> ExecutionMode.Xfb | 12u -> ExecutionMode.DepthReplacing | 14u -> ExecutionMode.DepthGreater | 15u -> ExecutionMode.DepthLess | 16u -> ExecutionMode.DepthUnchanged | 17u -> ExecutionMode.LocalSize(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 18u -> ExecutionMode.LocalSizeHint(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 19u -> ExecutionMode.InputPoints | 20u -> ExecutionMode.InputLines | 21u -> ExecutionMode.InputLinesAdjacency | 22u -> ExecutionMode.Triangles | 23u -> ExecutionMode.InputTrianglesAdjacency | 24u -> ExecutionMode.Quads | 25u -> ExecutionMode.Isolines | 26u -> ExecutionMode.OutputVertices(stream.ReadUInt32()) | 27u -> ExecutionMode.OutputPoints | 28u -> ExecutionMode.OutputLineStrip | 29u -> ExecutionMode.OutputTriangleStrip | 30u -> ExecutionMode.VecTypeHint(stream.ReadUInt32()) | 31u -> ExecutionMode.ContractionOff | 33u -> ExecutionMode.Initializer | 34u -> ExecutionMode.Finalizer | 35u -> ExecutionMode.SubgroupSize(stream.ReadUInt32()) | 36u -> ExecutionMode.SubgroupsPerWorkgroup(stream.ReadUInt32()) | 37u -> ExecutionMode.SubgroupsPerWorkgroupId(stream.ReadUInt32()) | 38u -> ExecutionMode.LocalSizeId(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 39u -> ExecutionMode.LocalSizeHintId(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 4169u -> ExecutionMode.NonCoherentColorAttachmentReadEXT | 4170u -> ExecutionMode.NonCoherentDepthAttachmentReadEXT | 4171u -> ExecutionMode.NonCoherentStencilAttachmentReadEXT | 4421u -> ExecutionMode.SubgroupUniformControlFlowKHR | 4446u -> ExecutionMode.PostDepthCoverage | 4459u -> ExecutionMode.DenormPreserve(stream.ReadUInt32()) | 4460u -> ExecutionMode.DenormFlushToZero(stream.ReadUInt32()) | 4461u -> ExecutionMode.SignedZeroInfNanPreserve(stream.ReadUInt32()) | 4462u -> ExecutionMode.RoundingModeRTE(stream.ReadUInt32()) | 4463u -> ExecutionMode.RoundingModeRTZ(stream.ReadUInt32()) | 5017u -> ExecutionMode.EarlyAndLateFragmentTestsAMD | 5027u -> ExecutionMode.StencilRefReplacingEXT | 5069u -> ExecutionMode.CoalescingAMDX | 5070u -> ExecutionMode.IsApiEntryAMDX(stream.ReadUInt32()) | 5071u -> ExecutionMode.MaxNodeRecursionAMDX(stream.ReadUInt32()) | 5072u -> ExecutionMode.StaticNumWorkgroupsAMDX(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 5073u -> ExecutionMode.ShaderIndexAMDX(stream.ReadUInt32()) | 5077u -> ExecutionMode.MaxNumWorkgroupsAMDX(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 5079u -> ExecutionMode.StencilRefUnchangedFrontAMD | 5080u -> ExecutionMode.StencilRefGreaterFrontAMD | 5081u -> ExecutionMode.StencilRefLessFrontAMD | 5082u -> ExecutionMode.StencilRefUnchangedBackAMD | 5083u -> ExecutionMode.StencilRefGreaterBackAMD | 5084u -> ExecutionMode.StencilRefLessBackAMD | 5088u -> ExecutionMode.QuadDerivativesKHR | 5089u -> ExecutionMode.RequireFullQuadsKHR | 5102u -> ExecutionMode.SharesInputWithAMDX(stream.ReadUInt32(), stream.ReadUInt32()) | 5269u -> ExecutionMode.OutputLinesEXT | 5270u -> ExecutionMode.OutputPrimitivesEXT(stream.ReadUInt32()) | 5289u -> ExecutionMode.DerivativeGroupQuadsKHR | 5290u -> ExecutionMode.DerivativeGroupLinearKHR | 5298u -> ExecutionMode.OutputTrianglesEXT | 5366u -> ExecutionMode.PixelInterlockOrderedEXT | 5367u -> ExecutionMode.PixelInterlockUnorderedEXT | 5368u -> ExecutionMode.SampleInterlockOrderedEXT | 5369u -> ExecutionMode.SampleInterlockUnorderedEXT | 5370u -> ExecutionMode.ShadingRateInterlockOrderedEXT | 5371u -> ExecutionMode.ShadingRateInterlockUnorderedEXT | 5618u -> ExecutionMode.SharedLocalMemorySizeINTEL(stream.ReadUInt32()) | 5620u -> ExecutionMode.RoundingModeRTPINTEL(stream.ReadUInt32()) | 5621u -> ExecutionMode.RoundingModeRTNINTEL(stream.ReadUInt32()) | 5622u -> ExecutionMode.FloatingPointModeALTINTEL(stream.ReadUInt32()) | 5623u -> ExecutionMode.FloatingPointModeIEEEINTEL(stream.ReadUInt32()) | 5893u -> ExecutionMode.MaxWorkgroupSizeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 5894u -> ExecutionMode.MaxWorkDimINTEL(stream.ReadUInt32()) | 5895u -> ExecutionMode.NoGlobalOffsetINTEL | 5896u -> ExecutionMode.NumSIMDWorkitemsINTEL(stream.ReadUInt32()) | 5903u -> ExecutionMode.SchedulerTargetFmaxMhzINTEL(stream.ReadUInt32()) | 6023u -> ExecutionMode.MaximallyReconvergesKHR | 6028u -> ExecutionMode.FPFastMathDefault(stream.ReadUInt32(), stream.ReadUInt32()) | 6154u -> ExecutionMode.StreamingInterfaceINTEL(stream.ReadUInt32()) | 6160u -> ExecutionMode.RegisterMapInterfaceINTEL(stream.ReadUInt32()) | 6417u -> ExecutionMode.NamedBarrierCountINTEL(stream.ReadUInt32()) | 6461u -> ExecutionMode.MaximumRegistersINTEL(stream.ReadUInt32()) | 6462u -> ExecutionMode.MaximumRegistersIdINTEL(stream.ReadUInt32()) | 6463u -> ExecutionMode.NamedMaximumRegistersINTEL(stream.ReadEnum()) | _ -> failwith "invalid" ))
        | 17us ->
            OpCapability(stream.ReadEnum())
        | 19us ->
            OpTypeVoid(stream.ReadUInt32())
        | 20us ->
            OpTypeBool(stream.ReadUInt32())
        | 21us ->
            OpTypeInt(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 22us ->
            OpTypeFloat(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadEnum()))
        | 23us ->
            OpTypeVector(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 24us ->
            OpTypeMatrix(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 25us ->
            OpTypeImage(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadOption(fun () -> stream.ReadEnum()))
        | 26us ->
            OpTypeSampler(stream.ReadUInt32())
        | 27us ->
            OpTypeSampledImage(stream.ReadUInt32(), stream.ReadUInt32())
        | 28us ->
            OpTypeArray(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 29us ->
            OpTypeRuntimeArray(stream.ReadUInt32(), stream.ReadUInt32())
        | 30us ->
            OpTypeStruct(stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 31us ->
            OpTypeOpaque(stream.ReadUInt32(), stream.ReadString())
        | 32us ->
            OpTypePointer(stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 33us ->
            OpTypeFunction(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 34us ->
            OpTypeEvent(stream.ReadUInt32())
        | 35us ->
            OpTypeDeviceEvent(stream.ReadUInt32())
        | 36us ->
            OpTypeReserveId(stream.ReadUInt32())
        | 37us ->
            OpTypeQueue(stream.ReadUInt32())
        | 38us ->
            OpTypePipe(stream.ReadUInt32(), stream.ReadEnum())
        | 39us ->
            OpTypeForwardPointer(stream.ReadUInt32(), stream.ReadEnum())
        | 41us ->
            OpConstantTrue(stream.ReadUInt32(), stream.ReadUInt32())
        | 42us ->
            OpConstantFalse(stream.ReadUInt32(), stream.ReadUInt32())
        | 43us ->
            OpConstant(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 44us ->
            OpConstantComposite(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 45us ->
            OpConstantSampler(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadEnum())
        | 46us ->
            OpConstantNull(stream.ReadUInt32(), stream.ReadUInt32())
        | 48us ->
            OpSpecConstantTrue(stream.ReadUInt32(), stream.ReadUInt32())
        | 49us ->
            OpSpecConstantFalse(stream.ReadUInt32(), stream.ReadUInt32())
        | 50us ->
            OpSpecConstant(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 51us ->
            OpSpecConstantComposite(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 52us ->
            OpSpecConstantOp(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 54us ->
            OpFunction(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 55us ->
            OpFunctionParameter(stream.ReadUInt32(), stream.ReadUInt32())
        | 56us ->
            OpFunctionEnd
        | 57us ->
            OpFunctionCall(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 59us ->
            OpVariable(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 60us ->
            OpImageTexelPointer(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 61us ->
            OpLoad(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 62us ->
            OpStore(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 63us ->
            OpCopyMemory(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 64us ->
            OpCopyMemorySized(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 65us ->
            OpAccessChain(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 66us ->
            OpInBoundsAccessChain(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 67us ->
            OpPtrAccessChain(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 68us ->
            OpArrayLength(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 69us ->
            OpGenericPtrMemSemantics(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 70us ->
            OpInBoundsPtrAccessChain(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 71us ->
            OpDecorate(stream.ReadUInt32(), (match stream.ReadUInt32() with | 0u -> Decoration.RelaxedPrecision | 1u -> Decoration.SpecId(stream.ReadUInt32()) | 2u -> Decoration.Block | 3u -> Decoration.BufferBlock | 4u -> Decoration.RowMajor | 5u -> Decoration.ColMajor | 6u -> Decoration.ArrayStride(stream.ReadUInt32()) | 7u -> Decoration.MatrixStride(stream.ReadUInt32()) | 8u -> Decoration.GLSLShared | 9u -> Decoration.GLSLPacked | 10u -> Decoration.CPacked | 11u -> Decoration.BuiltIn(stream.ReadEnum()) | 13u -> Decoration.NoPerspective | 14u -> Decoration.Flat | 15u -> Decoration.Patch | 16u -> Decoration.Centroid | 17u -> Decoration.Sample | 18u -> Decoration.Invariant | 19u -> Decoration.Restrict | 20u -> Decoration.Aliased | 21u -> Decoration.Volatile | 22u -> Decoration.Constant | 23u -> Decoration.Coherent | 24u -> Decoration.NonWritable | 25u -> Decoration.NonReadable | 26u -> Decoration.Uniform | 27u -> Decoration.UniformId(stream.ReadUInt32()) | 28u -> Decoration.SaturatedConversion | 29u -> Decoration.Stream(stream.ReadUInt32()) | 30u -> Decoration.Location(stream.ReadUInt32()) | 31u -> Decoration.Component(stream.ReadUInt32()) | 32u -> Decoration.Index(stream.ReadUInt32()) | 33u -> Decoration.Binding(stream.ReadUInt32()) | 34u -> Decoration.DescriptorSet(stream.ReadUInt32()) | 35u -> Decoration.Offset(stream.ReadUInt32()) | 36u -> Decoration.XfbBuffer(stream.ReadUInt32()) | 37u -> Decoration.XfbStride(stream.ReadUInt32()) | 38u -> Decoration.FuncParamAttr(stream.ReadEnum()) | 39u -> Decoration.FPRoundingMode(stream.ReadEnum()) | 40u -> Decoration.FPFastMathMode(stream.ReadEnum()) | 41u -> Decoration.LinkageAttributes(stream.ReadString(), stream.ReadEnum()) | 42u -> Decoration.NoContraction | 43u -> Decoration.InputAttachmentIndex(stream.ReadUInt32()) | 44u -> Decoration.Alignment(stream.ReadUInt32()) | 45u -> Decoration.MaxByteOffset(stream.ReadUInt32()) | 46u -> Decoration.AlignmentId(stream.ReadUInt32()) | 47u -> Decoration.MaxByteOffsetId(stream.ReadUInt32()) | 4469u -> Decoration.NoSignedWrap | 4470u -> Decoration.NoUnsignedWrap | 4487u -> Decoration.WeightTextureQCOM | 4488u -> Decoration.BlockMatchTextureQCOM | 4499u -> Decoration.BlockMatchSamplerQCOM | 4999u -> Decoration.ExplicitInterpAMD | 5019u -> Decoration.NodeSharesPayloadLimitsWithAMDX(stream.ReadUInt32()) | 5020u -> Decoration.NodeMaxPayloadsAMDX(stream.ReadUInt32()) | 5078u -> Decoration.TrackFinishWritingAMDX | 5091u -> Decoration.PayloadNodeNameAMDX(stream.ReadUInt32()) | 5098u -> Decoration.PayloadNodeBaseIndexAMDX(stream.ReadUInt32()) | 5099u -> Decoration.PayloadNodeSparseArrayAMDX | 5100u -> Decoration.PayloadNodeArraySizeAMDX(stream.ReadUInt32()) | 5105u -> Decoration.PayloadDispatchIndirectAMDX | 5248u -> Decoration.OverrideCoverageNV | 5250u -> Decoration.PassthroughNV | 5252u -> Decoration.ViewportRelativeNV | 5256u -> Decoration.SecondaryViewportRelativeNV(stream.ReadUInt32()) | 5271u -> Decoration.PerPrimitiveEXT | 5272u -> Decoration.PerViewNV | 5273u -> Decoration.PerTaskNV | 5285u -> Decoration.PerVertexKHR | 5300u -> Decoration.NonUniform | 5355u -> Decoration.RestrictPointer | 5356u -> Decoration.AliasedPointer | 5386u -> Decoration.HitObjectShaderRecordBufferNV | 5398u -> Decoration.BindlessSamplerNV | 5399u -> Decoration.BindlessImageNV | 5400u -> Decoration.BoundSamplerNV | 5401u -> Decoration.BoundImageNV | 5599u -> Decoration.SIMTCallINTEL(stream.ReadUInt32()) | 5602u -> Decoration.ReferencedIndirectlyINTEL | 5607u -> Decoration.ClobberINTEL(stream.ReadString()) | 5608u -> Decoration.SideEffectsINTEL | 5624u -> Decoration.VectorComputeVariableINTEL | 5625u -> Decoration.FuncParamIOKindINTEL(stream.ReadUInt32()) | 5626u -> Decoration.VectorComputeFunctionINTEL | 5627u -> Decoration.StackCallINTEL | 5628u -> Decoration.GlobalVariableOffsetINTEL(stream.ReadUInt32()) | 5634u -> Decoration.CounterBuffer(stream.ReadUInt32()) | 5635u -> Decoration.UserSemantic(stream.ReadString()) | 5636u -> Decoration.UserTypeGOOGLE(stream.ReadString()) | 5822u -> Decoration.FunctionRoundingModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 5823u -> Decoration.FunctionDenormModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 5825u -> Decoration.RegisterINTEL | 5826u -> Decoration.MemoryINTEL(stream.ReadString()) | 5827u -> Decoration.NumbanksINTEL(stream.ReadUInt32()) | 5828u -> Decoration.BankwidthINTEL(stream.ReadUInt32()) | 5829u -> Decoration.MaxPrivateCopiesINTEL(stream.ReadUInt32()) | 5830u -> Decoration.SinglepumpINTEL | 5831u -> Decoration.DoublepumpINTEL | 5832u -> Decoration.MaxReplicatesINTEL(stream.ReadUInt32()) | 5833u -> Decoration.SimpleDualPortINTEL | 5834u -> Decoration.MergeINTEL(stream.ReadString(), stream.ReadString()) | 5835u -> Decoration.BankBitsINTEL(stream.ReadUInt32()) | 5836u -> Decoration.ForcePow2DepthINTEL(stream.ReadUInt32()) | 5883u -> Decoration.StridesizeINTEL(stream.ReadUInt32()) | 5884u -> Decoration.WordsizeINTEL(stream.ReadUInt32()) | 5885u -> Decoration.TrueDualPortINTEL | 5899u -> Decoration.BurstCoalesceINTEL | 5900u -> Decoration.CacheSizeINTEL(stream.ReadUInt32()) | 5901u -> Decoration.DontStaticallyCoalesceINTEL | 5902u -> Decoration.PrefetchINTEL(stream.ReadUInt32()) | 5905u -> Decoration.StallEnableINTEL | 5907u -> Decoration.FuseLoopsInFunctionINTEL | 5909u -> Decoration.MathOpDSPModeINTEL(stream.ReadUInt32(), stream.ReadUInt32()) | 5914u -> Decoration.AliasScopeINTEL(stream.ReadUInt32()) | 5915u -> Decoration.NoAliasINTEL(stream.ReadUInt32()) | 5917u -> Decoration.InitiationIntervalINTEL(stream.ReadUInt32()) | 5918u -> Decoration.MaxConcurrencyINTEL(stream.ReadUInt32()) | 5919u -> Decoration.PipelineEnableINTEL(stream.ReadUInt32()) | 5921u -> Decoration.BufferLocationINTEL(stream.ReadUInt32()) | 5944u -> Decoration.IOPipeStorageINTEL(stream.ReadUInt32()) | 6080u -> Decoration.FunctionFloatingPointModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 6085u -> Decoration.SingleElementVectorINTEL | 6087u -> Decoration.VectorComputeCallableFunctionINTEL | 6140u -> Decoration.MediaBlockIOINTEL | 6151u -> Decoration.StallFreeINTEL | 6170u -> Decoration.FPMaxErrorDecorationINTEL(stream.ReadUInt32()) | 6172u -> Decoration.LatencyControlLabelINTEL(stream.ReadUInt32()) | 6173u -> Decoration.LatencyControlConstraintINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 6175u -> Decoration.ConduitKernelArgumentINTEL | 6176u -> Decoration.RegisterMapKernelArgumentINTEL | 6177u -> Decoration.MMHostInterfaceAddressWidthINTEL(stream.ReadUInt32()) | 6178u -> Decoration.MMHostInterfaceDataWidthINTEL(stream.ReadUInt32()) | 6179u -> Decoration.MMHostInterfaceLatencyINTEL(stream.ReadUInt32()) | 6180u -> Decoration.MMHostInterfaceReadWriteModeINTEL(stream.ReadEnum()) | 6181u -> Decoration.MMHostInterfaceMaxBurstINTEL(stream.ReadUInt32()) | 6182u -> Decoration.MMHostInterfaceWaitRequestINTEL(stream.ReadUInt32()) | 6183u -> Decoration.StableKernelArgumentINTEL | 6188u -> Decoration.HostAccessINTEL(stream.ReadEnum(), stream.ReadString()) | 6190u -> Decoration.InitModeINTEL(stream.ReadEnum()) | 6191u -> Decoration.ImplementInRegisterMapINTEL(stream.ReadUInt32()) | 6442u -> Decoration.CacheControlLoadINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 6443u -> Decoration.CacheControlStoreINTEL(stream.ReadUInt32(), stream.ReadEnum()) | _ -> failwith "invalid" ))
        | 72us ->
            OpMemberDecorate(stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0u -> Decoration.RelaxedPrecision | 1u -> Decoration.SpecId(stream.ReadUInt32()) | 2u -> Decoration.Block | 3u -> Decoration.BufferBlock | 4u -> Decoration.RowMajor | 5u -> Decoration.ColMajor | 6u -> Decoration.ArrayStride(stream.ReadUInt32()) | 7u -> Decoration.MatrixStride(stream.ReadUInt32()) | 8u -> Decoration.GLSLShared | 9u -> Decoration.GLSLPacked | 10u -> Decoration.CPacked | 11u -> Decoration.BuiltIn(stream.ReadEnum()) | 13u -> Decoration.NoPerspective | 14u -> Decoration.Flat | 15u -> Decoration.Patch | 16u -> Decoration.Centroid | 17u -> Decoration.Sample | 18u -> Decoration.Invariant | 19u -> Decoration.Restrict | 20u -> Decoration.Aliased | 21u -> Decoration.Volatile | 22u -> Decoration.Constant | 23u -> Decoration.Coherent | 24u -> Decoration.NonWritable | 25u -> Decoration.NonReadable | 26u -> Decoration.Uniform | 27u -> Decoration.UniformId(stream.ReadUInt32()) | 28u -> Decoration.SaturatedConversion | 29u -> Decoration.Stream(stream.ReadUInt32()) | 30u -> Decoration.Location(stream.ReadUInt32()) | 31u -> Decoration.Component(stream.ReadUInt32()) | 32u -> Decoration.Index(stream.ReadUInt32()) | 33u -> Decoration.Binding(stream.ReadUInt32()) | 34u -> Decoration.DescriptorSet(stream.ReadUInt32()) | 35u -> Decoration.Offset(stream.ReadUInt32()) | 36u -> Decoration.XfbBuffer(stream.ReadUInt32()) | 37u -> Decoration.XfbStride(stream.ReadUInt32()) | 38u -> Decoration.FuncParamAttr(stream.ReadEnum()) | 39u -> Decoration.FPRoundingMode(stream.ReadEnum()) | 40u -> Decoration.FPFastMathMode(stream.ReadEnum()) | 41u -> Decoration.LinkageAttributes(stream.ReadString(), stream.ReadEnum()) | 42u -> Decoration.NoContraction | 43u -> Decoration.InputAttachmentIndex(stream.ReadUInt32()) | 44u -> Decoration.Alignment(stream.ReadUInt32()) | 45u -> Decoration.MaxByteOffset(stream.ReadUInt32()) | 46u -> Decoration.AlignmentId(stream.ReadUInt32()) | 47u -> Decoration.MaxByteOffsetId(stream.ReadUInt32()) | 4469u -> Decoration.NoSignedWrap | 4470u -> Decoration.NoUnsignedWrap | 4487u -> Decoration.WeightTextureQCOM | 4488u -> Decoration.BlockMatchTextureQCOM | 4499u -> Decoration.BlockMatchSamplerQCOM | 4999u -> Decoration.ExplicitInterpAMD | 5019u -> Decoration.NodeSharesPayloadLimitsWithAMDX(stream.ReadUInt32()) | 5020u -> Decoration.NodeMaxPayloadsAMDX(stream.ReadUInt32()) | 5078u -> Decoration.TrackFinishWritingAMDX | 5091u -> Decoration.PayloadNodeNameAMDX(stream.ReadUInt32()) | 5098u -> Decoration.PayloadNodeBaseIndexAMDX(stream.ReadUInt32()) | 5099u -> Decoration.PayloadNodeSparseArrayAMDX | 5100u -> Decoration.PayloadNodeArraySizeAMDX(stream.ReadUInt32()) | 5105u -> Decoration.PayloadDispatchIndirectAMDX | 5248u -> Decoration.OverrideCoverageNV | 5250u -> Decoration.PassthroughNV | 5252u -> Decoration.ViewportRelativeNV | 5256u -> Decoration.SecondaryViewportRelativeNV(stream.ReadUInt32()) | 5271u -> Decoration.PerPrimitiveEXT | 5272u -> Decoration.PerViewNV | 5273u -> Decoration.PerTaskNV | 5285u -> Decoration.PerVertexKHR | 5300u -> Decoration.NonUniform | 5355u -> Decoration.RestrictPointer | 5356u -> Decoration.AliasedPointer | 5386u -> Decoration.HitObjectShaderRecordBufferNV | 5398u -> Decoration.BindlessSamplerNV | 5399u -> Decoration.BindlessImageNV | 5400u -> Decoration.BoundSamplerNV | 5401u -> Decoration.BoundImageNV | 5599u -> Decoration.SIMTCallINTEL(stream.ReadUInt32()) | 5602u -> Decoration.ReferencedIndirectlyINTEL | 5607u -> Decoration.ClobberINTEL(stream.ReadString()) | 5608u -> Decoration.SideEffectsINTEL | 5624u -> Decoration.VectorComputeVariableINTEL | 5625u -> Decoration.FuncParamIOKindINTEL(stream.ReadUInt32()) | 5626u -> Decoration.VectorComputeFunctionINTEL | 5627u -> Decoration.StackCallINTEL | 5628u -> Decoration.GlobalVariableOffsetINTEL(stream.ReadUInt32()) | 5634u -> Decoration.CounterBuffer(stream.ReadUInt32()) | 5635u -> Decoration.UserSemantic(stream.ReadString()) | 5636u -> Decoration.UserTypeGOOGLE(stream.ReadString()) | 5822u -> Decoration.FunctionRoundingModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 5823u -> Decoration.FunctionDenormModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 5825u -> Decoration.RegisterINTEL | 5826u -> Decoration.MemoryINTEL(stream.ReadString()) | 5827u -> Decoration.NumbanksINTEL(stream.ReadUInt32()) | 5828u -> Decoration.BankwidthINTEL(stream.ReadUInt32()) | 5829u -> Decoration.MaxPrivateCopiesINTEL(stream.ReadUInt32()) | 5830u -> Decoration.SinglepumpINTEL | 5831u -> Decoration.DoublepumpINTEL | 5832u -> Decoration.MaxReplicatesINTEL(stream.ReadUInt32()) | 5833u -> Decoration.SimpleDualPortINTEL | 5834u -> Decoration.MergeINTEL(stream.ReadString(), stream.ReadString()) | 5835u -> Decoration.BankBitsINTEL(stream.ReadUInt32()) | 5836u -> Decoration.ForcePow2DepthINTEL(stream.ReadUInt32()) | 5883u -> Decoration.StridesizeINTEL(stream.ReadUInt32()) | 5884u -> Decoration.WordsizeINTEL(stream.ReadUInt32()) | 5885u -> Decoration.TrueDualPortINTEL | 5899u -> Decoration.BurstCoalesceINTEL | 5900u -> Decoration.CacheSizeINTEL(stream.ReadUInt32()) | 5901u -> Decoration.DontStaticallyCoalesceINTEL | 5902u -> Decoration.PrefetchINTEL(stream.ReadUInt32()) | 5905u -> Decoration.StallEnableINTEL | 5907u -> Decoration.FuseLoopsInFunctionINTEL | 5909u -> Decoration.MathOpDSPModeINTEL(stream.ReadUInt32(), stream.ReadUInt32()) | 5914u -> Decoration.AliasScopeINTEL(stream.ReadUInt32()) | 5915u -> Decoration.NoAliasINTEL(stream.ReadUInt32()) | 5917u -> Decoration.InitiationIntervalINTEL(stream.ReadUInt32()) | 5918u -> Decoration.MaxConcurrencyINTEL(stream.ReadUInt32()) | 5919u -> Decoration.PipelineEnableINTEL(stream.ReadUInt32()) | 5921u -> Decoration.BufferLocationINTEL(stream.ReadUInt32()) | 5944u -> Decoration.IOPipeStorageINTEL(stream.ReadUInt32()) | 6080u -> Decoration.FunctionFloatingPointModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 6085u -> Decoration.SingleElementVectorINTEL | 6087u -> Decoration.VectorComputeCallableFunctionINTEL | 6140u -> Decoration.MediaBlockIOINTEL | 6151u -> Decoration.StallFreeINTEL | 6170u -> Decoration.FPMaxErrorDecorationINTEL(stream.ReadUInt32()) | 6172u -> Decoration.LatencyControlLabelINTEL(stream.ReadUInt32()) | 6173u -> Decoration.LatencyControlConstraintINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 6175u -> Decoration.ConduitKernelArgumentINTEL | 6176u -> Decoration.RegisterMapKernelArgumentINTEL | 6177u -> Decoration.MMHostInterfaceAddressWidthINTEL(stream.ReadUInt32()) | 6178u -> Decoration.MMHostInterfaceDataWidthINTEL(stream.ReadUInt32()) | 6179u -> Decoration.MMHostInterfaceLatencyINTEL(stream.ReadUInt32()) | 6180u -> Decoration.MMHostInterfaceReadWriteModeINTEL(stream.ReadEnum()) | 6181u -> Decoration.MMHostInterfaceMaxBurstINTEL(stream.ReadUInt32()) | 6182u -> Decoration.MMHostInterfaceWaitRequestINTEL(stream.ReadUInt32()) | 6183u -> Decoration.StableKernelArgumentINTEL | 6188u -> Decoration.HostAccessINTEL(stream.ReadEnum(), stream.ReadString()) | 6190u -> Decoration.InitModeINTEL(stream.ReadEnum()) | 6191u -> Decoration.ImplementInRegisterMapINTEL(stream.ReadUInt32()) | 6442u -> Decoration.CacheControlLoadINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 6443u -> Decoration.CacheControlStoreINTEL(stream.ReadUInt32(), stream.ReadEnum()) | _ -> failwith "invalid" ))
        | 73us ->
            OpDecorationGroup(stream.ReadUInt32())
        | 74us ->
            OpGroupDecorate(stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 75us ->
            OpGroupMemberDecorate(stream.ReadUInt32(), stream.ReadList(fun () -> PairIdRefLiteralInteger(stream.ReadUInt32(), stream.ReadUInt32())))
        | 77us ->
            OpVectorExtractDynamic(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 78us ->
            OpVectorInsertDynamic(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 79us ->
            OpVectorShuffle(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 80us ->
            OpCompositeConstruct(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 81us ->
            OpCompositeExtract(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 82us ->
            OpCompositeInsert(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 83us ->
            OpCopyObject(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 84us ->
            OpTranspose(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 86us ->
            OpSampledImage(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 87us ->
            OpImageSampleImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 88us ->
            OpImageSampleExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 89us ->
            OpImageSampleDrefImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 90us ->
            OpImageSampleDrefExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 91us ->
            OpImageSampleProjImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 92us ->
            OpImageSampleProjExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 93us ->
            OpImageSampleProjDrefImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 94us ->
            OpImageSampleProjDrefExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 95us ->
            OpImageFetch(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 96us ->
            OpImageGather(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 97us ->
            OpImageDrefGather(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 98us ->
            OpImageRead(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 99us ->
            OpImageWrite(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 100us ->
            OpImage(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 101us ->
            OpImageQueryFormat(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 102us ->
            OpImageQueryOrder(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 103us ->
            OpImageQuerySizeLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 104us ->
            OpImageQuerySize(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 105us ->
            OpImageQueryLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 106us ->
            OpImageQueryLevels(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 107us ->
            OpImageQuerySamples(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 109us ->
            OpConvertFToU(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 110us ->
            OpConvertFToS(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 111us ->
            OpConvertSToF(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 112us ->
            OpConvertUToF(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 113us ->
            OpUConvert(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 114us ->
            OpSConvert(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 115us ->
            OpFConvert(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 116us ->
            OpQuantizeToF16(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 117us ->
            OpConvertPtrToU(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 118us ->
            OpSatConvertSToU(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 119us ->
            OpSatConvertUToS(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 120us ->
            OpConvertUToPtr(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 121us ->
            OpPtrCastToGeneric(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 122us ->
            OpGenericCastToPtr(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 123us ->
            OpGenericCastToPtrExplicit(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum())
        | 124us ->
            OpBitcast(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 126us ->
            OpSNegate(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 127us ->
            OpFNegate(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 128us ->
            OpIAdd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 129us ->
            OpFAdd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 130us ->
            OpISub(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 131us ->
            OpFSub(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 132us ->
            OpIMul(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 133us ->
            OpFMul(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 134us ->
            OpUDiv(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 135us ->
            OpSDiv(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 136us ->
            OpFDiv(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 137us ->
            OpUMod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 138us ->
            OpSRem(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 139us ->
            OpSMod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 140us ->
            OpFRem(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 141us ->
            OpFMod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 142us ->
            OpVectorTimesScalar(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 143us ->
            OpMatrixTimesScalar(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 144us ->
            OpVectorTimesMatrix(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 145us ->
            OpMatrixTimesVector(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 146us ->
            OpMatrixTimesMatrix(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 147us ->
            OpOuterProduct(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 148us ->
            OpDot(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 149us ->
            OpIAddCarry(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 150us ->
            OpISubBorrow(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 151us ->
            OpUMulExtended(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 152us ->
            OpSMulExtended(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 154us ->
            OpAny(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 155us ->
            OpAll(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 156us ->
            OpIsNan(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 157us ->
            OpIsInf(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 158us ->
            OpIsFinite(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 159us ->
            OpIsNormal(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 160us ->
            OpSignBitSet(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 161us ->
            OpLessOrGreater(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 162us ->
            OpOrdered(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 163us ->
            OpUnordered(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 164us ->
            OpLogicalEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 165us ->
            OpLogicalNotEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 166us ->
            OpLogicalOr(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 167us ->
            OpLogicalAnd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 168us ->
            OpLogicalNot(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 169us ->
            OpSelect(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 170us ->
            OpIEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 171us ->
            OpINotEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 172us ->
            OpUGreaterThan(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 173us ->
            OpSGreaterThan(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 174us ->
            OpUGreaterThanEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 175us ->
            OpSGreaterThanEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 176us ->
            OpULessThan(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 177us ->
            OpSLessThan(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 178us ->
            OpULessThanEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 179us ->
            OpSLessThanEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 180us ->
            OpFOrdEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 181us ->
            OpFUnordEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 182us ->
            OpFOrdNotEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 183us ->
            OpFUnordNotEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 184us ->
            OpFOrdLessThan(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 185us ->
            OpFUnordLessThan(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 186us ->
            OpFOrdGreaterThan(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 187us ->
            OpFUnordGreaterThan(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 188us ->
            OpFOrdLessThanEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 189us ->
            OpFUnordLessThanEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 190us ->
            OpFOrdGreaterThanEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 191us ->
            OpFUnordGreaterThanEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 194us ->
            OpShiftRightLogical(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 195us ->
            OpShiftRightArithmetic(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 196us ->
            OpShiftLeftLogical(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 197us ->
            OpBitwiseOr(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 198us ->
            OpBitwiseXor(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 199us ->
            OpBitwiseAnd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 200us ->
            OpNot(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 201us ->
            OpBitFieldInsert(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 202us ->
            OpBitFieldSExtract(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 203us ->
            OpBitFieldUExtract(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 204us ->
            OpBitReverse(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 205us ->
            OpBitCount(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 207us ->
            OpDPdx(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 208us ->
            OpDPdy(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 209us ->
            OpFwidth(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 210us ->
            OpDPdxFine(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 211us ->
            OpDPdyFine(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 212us ->
            OpFwidthFine(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 213us ->
            OpDPdxCoarse(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 214us ->
            OpDPdyCoarse(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 215us ->
            OpFwidthCoarse(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 218us ->
            OpEmitVertex
        | 219us ->
            OpEndPrimitive
        | 220us ->
            OpEmitStreamVertex(stream.ReadUInt32())
        | 221us ->
            OpEndStreamPrimitive(stream.ReadUInt32())
        | 224us ->
            OpControlBarrier(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 225us ->
            OpMemoryBarrier(stream.ReadUInt32(), stream.ReadUInt32())
        | 227us ->
            OpAtomicLoad(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 228us ->
            OpAtomicStore(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 229us ->
            OpAtomicExchange(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 230us ->
            OpAtomicCompareExchange(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 231us ->
            OpAtomicCompareExchangeWeak(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 232us ->
            OpAtomicIIncrement(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 233us ->
            OpAtomicIDecrement(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 234us ->
            OpAtomicIAdd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 235us ->
            OpAtomicISub(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 236us ->
            OpAtomicSMin(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 237us ->
            OpAtomicUMin(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 238us ->
            OpAtomicSMax(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 239us ->
            OpAtomicUMax(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 240us ->
            OpAtomicAnd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 241us ->
            OpAtomicOr(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 242us ->
            OpAtomicXor(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 245us ->
            OpPhi(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> PairIdRefIdRef(stream.ReadUInt32(), stream.ReadUInt32())))
        | 246us ->
            OpLoopMerge(stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> LoopControl.None | 0x0001u -> LoopControl.Unroll | 0x0002u -> LoopControl.DontUnroll | 0x0004u -> LoopControl.DependencyInfinite | 0x0008u -> LoopControl.DependencyLength(stream.ReadUInt32()) | 0x0010u -> LoopControl.MinIterations(stream.ReadUInt32()) | 0x0020u -> LoopControl.MaxIterations(stream.ReadUInt32()) | 0x0040u -> LoopControl.IterationMultiple(stream.ReadUInt32()) | 0x0080u -> LoopControl.PeelCount(stream.ReadUInt32()) | 0x0100u -> LoopControl.PartialCount(stream.ReadUInt32()) | 0x10000u -> LoopControl.InitiationIntervalINTEL(stream.ReadUInt32()) | 0x20000u -> LoopControl.MaxConcurrencyINTEL(stream.ReadUInt32()) | 0x40000u -> LoopControl.DependencyArrayINTEL(stream.ReadUInt32()) | 0x80000u -> LoopControl.PipelineEnableINTEL(stream.ReadUInt32()) | 0x100000u -> LoopControl.LoopCoalesceINTEL(stream.ReadUInt32()) | 0x200000u -> LoopControl.MaxInterleavingINTEL(stream.ReadUInt32()) | 0x400000u -> LoopControl.SpeculatedIterationsINTEL(stream.ReadUInt32()) | 0x800000u -> LoopControl.NoFusionINTEL | 0x1000000u -> LoopControl.LoopCountINTEL(stream.ReadUInt32()) | 0x2000000u -> LoopControl.MaxReinvocationDelayINTEL(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 247us ->
            OpSelectionMerge(stream.ReadUInt32(), stream.ReadEnum())
        | 248us ->
            OpLabel(stream.ReadUInt32())
        | 249us ->
            OpBranch(stream.ReadUInt32())
        | 250us ->
            OpBranchConditional(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 251us ->
            OpSwitch(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> PairLiteralIntegerIdRef(stream.ReadUInt32(), stream.ReadUInt32())))
        | 252us ->
            OpKill
        | 253us ->
            OpReturn
        | 254us ->
            OpReturnValue(stream.ReadUInt32())
        | 255us ->
            OpUnreachable
        | 256us ->
            OpLifetimeStart(stream.ReadUInt32(), stream.ReadUInt32())
        | 257us ->
            OpLifetimeStop(stream.ReadUInt32(), stream.ReadUInt32())
        | 259us ->
            OpGroupAsyncCopy(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 260us ->
            OpGroupWaitEvents(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 261us ->
            OpGroupAll(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 262us ->
            OpGroupAny(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 263us ->
            OpGroupBroadcast(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 264us ->
            OpGroupIAdd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 265us ->
            OpGroupFAdd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 266us ->
            OpGroupFMin(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 267us ->
            OpGroupUMin(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 268us ->
            OpGroupSMin(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 269us ->
            OpGroupFMax(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 270us ->
            OpGroupUMax(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 271us ->
            OpGroupSMax(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 274us ->
            OpReadPipe(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 275us ->
            OpWritePipe(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 276us ->
            OpReservedReadPipe(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 277us ->
            OpReservedWritePipe(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 278us ->
            OpReserveReadPipePackets(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 279us ->
            OpReserveWritePipePackets(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 280us ->
            OpCommitReadPipe(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 281us ->
            OpCommitWritePipe(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 282us ->
            OpIsValidReserveId(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 283us ->
            OpGetNumPipePackets(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 284us ->
            OpGetMaxPipePackets(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 285us ->
            OpGroupReserveReadPipePackets(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 286us ->
            OpGroupReserveWritePipePackets(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 287us ->
            OpGroupCommitReadPipe(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 288us ->
            OpGroupCommitWritePipe(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 291us ->
            OpEnqueueMarker(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 292us ->
            OpEnqueueKernel(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 293us ->
            OpGetKernelNDrangeSubGroupCount(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 294us ->
            OpGetKernelNDrangeMaxSubGroupSize(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 295us ->
            OpGetKernelWorkGroupSize(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 296us ->
            OpGetKernelPreferredWorkGroupSizeMultiple(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 297us ->
            OpRetainEvent(stream.ReadUInt32())
        | 298us ->
            OpReleaseEvent(stream.ReadUInt32())
        | 299us ->
            OpCreateUserEvent(stream.ReadUInt32(), stream.ReadUInt32())
        | 300us ->
            OpIsValidEvent(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 301us ->
            OpSetUserEventStatus(stream.ReadUInt32(), stream.ReadUInt32())
        | 302us ->
            OpCaptureEventProfilingInfo(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 303us ->
            OpGetDefaultQueue(stream.ReadUInt32(), stream.ReadUInt32())
        | 304us ->
            OpBuildNDRange(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 305us ->
            OpImageSparseSampleImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 306us ->
            OpImageSparseSampleExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 307us ->
            OpImageSparseSampleDrefImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 308us ->
            OpImageSparseSampleDrefExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 309us ->
            OpImageSparseSampleProjImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 310us ->
            OpImageSparseSampleProjExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 311us ->
            OpImageSparseSampleProjDrefImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 312us ->
            OpImageSparseSampleProjDrefExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 313us ->
            OpImageSparseFetch(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 314us ->
            OpImageSparseGather(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 315us ->
            OpImageSparseDrefGather(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 316us ->
            OpImageSparseTexelsResident(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 317us ->
            OpNoLine
        | 318us ->
            OpAtomicFlagTestAndSet(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 319us ->
            OpAtomicFlagClear(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 320us ->
            OpImageSparseRead(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 321us ->
            OpSizeOf(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 322us ->
            OpTypePipeStorage(stream.ReadUInt32())
        | 323us ->
            OpConstantPipeStorage(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 324us ->
            OpCreatePipeFromPipeStorage(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 325us ->
            OpGetKernelLocalSizeForSubgroupCount(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 326us ->
            OpGetKernelMaxNumSubgroups(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 327us ->
            OpTypeNamedBarrier(stream.ReadUInt32())
        | 328us ->
            OpNamedBarrierInitialize(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 329us ->
            OpMemoryNamedBarrier(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 330us ->
            OpModuleProcessed(stream.ReadString())
        | 331us ->
            OpExecutionModeId(stream.ReadUInt32(), (match stream.ReadUInt32() with | 0u -> ExecutionMode.Invocations(stream.ReadUInt32()) | 1u -> ExecutionMode.SpacingEqual | 2u -> ExecutionMode.SpacingFractionalEven | 3u -> ExecutionMode.SpacingFractionalOdd | 4u -> ExecutionMode.VertexOrderCw | 5u -> ExecutionMode.VertexOrderCcw | 6u -> ExecutionMode.PixelCenterInteger | 7u -> ExecutionMode.OriginUpperLeft | 8u -> ExecutionMode.OriginLowerLeft | 9u -> ExecutionMode.EarlyFragmentTests | 10u -> ExecutionMode.PointMode | 11u -> ExecutionMode.Xfb | 12u -> ExecutionMode.DepthReplacing | 14u -> ExecutionMode.DepthGreater | 15u -> ExecutionMode.DepthLess | 16u -> ExecutionMode.DepthUnchanged | 17u -> ExecutionMode.LocalSize(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 18u -> ExecutionMode.LocalSizeHint(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 19u -> ExecutionMode.InputPoints | 20u -> ExecutionMode.InputLines | 21u -> ExecutionMode.InputLinesAdjacency | 22u -> ExecutionMode.Triangles | 23u -> ExecutionMode.InputTrianglesAdjacency | 24u -> ExecutionMode.Quads | 25u -> ExecutionMode.Isolines | 26u -> ExecutionMode.OutputVertices(stream.ReadUInt32()) | 27u -> ExecutionMode.OutputPoints | 28u -> ExecutionMode.OutputLineStrip | 29u -> ExecutionMode.OutputTriangleStrip | 30u -> ExecutionMode.VecTypeHint(stream.ReadUInt32()) | 31u -> ExecutionMode.ContractionOff | 33u -> ExecutionMode.Initializer | 34u -> ExecutionMode.Finalizer | 35u -> ExecutionMode.SubgroupSize(stream.ReadUInt32()) | 36u -> ExecutionMode.SubgroupsPerWorkgroup(stream.ReadUInt32()) | 37u -> ExecutionMode.SubgroupsPerWorkgroupId(stream.ReadUInt32()) | 38u -> ExecutionMode.LocalSizeId(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 39u -> ExecutionMode.LocalSizeHintId(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 4169u -> ExecutionMode.NonCoherentColorAttachmentReadEXT | 4170u -> ExecutionMode.NonCoherentDepthAttachmentReadEXT | 4171u -> ExecutionMode.NonCoherentStencilAttachmentReadEXT | 4421u -> ExecutionMode.SubgroupUniformControlFlowKHR | 4446u -> ExecutionMode.PostDepthCoverage | 4459u -> ExecutionMode.DenormPreserve(stream.ReadUInt32()) | 4460u -> ExecutionMode.DenormFlushToZero(stream.ReadUInt32()) | 4461u -> ExecutionMode.SignedZeroInfNanPreserve(stream.ReadUInt32()) | 4462u -> ExecutionMode.RoundingModeRTE(stream.ReadUInt32()) | 4463u -> ExecutionMode.RoundingModeRTZ(stream.ReadUInt32()) | 5017u -> ExecutionMode.EarlyAndLateFragmentTestsAMD | 5027u -> ExecutionMode.StencilRefReplacingEXT | 5069u -> ExecutionMode.CoalescingAMDX | 5070u -> ExecutionMode.IsApiEntryAMDX(stream.ReadUInt32()) | 5071u -> ExecutionMode.MaxNodeRecursionAMDX(stream.ReadUInt32()) | 5072u -> ExecutionMode.StaticNumWorkgroupsAMDX(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 5073u -> ExecutionMode.ShaderIndexAMDX(stream.ReadUInt32()) | 5077u -> ExecutionMode.MaxNumWorkgroupsAMDX(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 5079u -> ExecutionMode.StencilRefUnchangedFrontAMD | 5080u -> ExecutionMode.StencilRefGreaterFrontAMD | 5081u -> ExecutionMode.StencilRefLessFrontAMD | 5082u -> ExecutionMode.StencilRefUnchangedBackAMD | 5083u -> ExecutionMode.StencilRefGreaterBackAMD | 5084u -> ExecutionMode.StencilRefLessBackAMD | 5088u -> ExecutionMode.QuadDerivativesKHR | 5089u -> ExecutionMode.RequireFullQuadsKHR | 5102u -> ExecutionMode.SharesInputWithAMDX(stream.ReadUInt32(), stream.ReadUInt32()) | 5269u -> ExecutionMode.OutputLinesEXT | 5270u -> ExecutionMode.OutputPrimitivesEXT(stream.ReadUInt32()) | 5289u -> ExecutionMode.DerivativeGroupQuadsKHR | 5290u -> ExecutionMode.DerivativeGroupLinearKHR | 5298u -> ExecutionMode.OutputTrianglesEXT | 5366u -> ExecutionMode.PixelInterlockOrderedEXT | 5367u -> ExecutionMode.PixelInterlockUnorderedEXT | 5368u -> ExecutionMode.SampleInterlockOrderedEXT | 5369u -> ExecutionMode.SampleInterlockUnorderedEXT | 5370u -> ExecutionMode.ShadingRateInterlockOrderedEXT | 5371u -> ExecutionMode.ShadingRateInterlockUnorderedEXT | 5618u -> ExecutionMode.SharedLocalMemorySizeINTEL(stream.ReadUInt32()) | 5620u -> ExecutionMode.RoundingModeRTPINTEL(stream.ReadUInt32()) | 5621u -> ExecutionMode.RoundingModeRTNINTEL(stream.ReadUInt32()) | 5622u -> ExecutionMode.FloatingPointModeALTINTEL(stream.ReadUInt32()) | 5623u -> ExecutionMode.FloatingPointModeIEEEINTEL(stream.ReadUInt32()) | 5893u -> ExecutionMode.MaxWorkgroupSizeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 5894u -> ExecutionMode.MaxWorkDimINTEL(stream.ReadUInt32()) | 5895u -> ExecutionMode.NoGlobalOffsetINTEL | 5896u -> ExecutionMode.NumSIMDWorkitemsINTEL(stream.ReadUInt32()) | 5903u -> ExecutionMode.SchedulerTargetFmaxMhzINTEL(stream.ReadUInt32()) | 6023u -> ExecutionMode.MaximallyReconvergesKHR | 6028u -> ExecutionMode.FPFastMathDefault(stream.ReadUInt32(), stream.ReadUInt32()) | 6154u -> ExecutionMode.StreamingInterfaceINTEL(stream.ReadUInt32()) | 6160u -> ExecutionMode.RegisterMapInterfaceINTEL(stream.ReadUInt32()) | 6417u -> ExecutionMode.NamedBarrierCountINTEL(stream.ReadUInt32()) | 6461u -> ExecutionMode.MaximumRegistersINTEL(stream.ReadUInt32()) | 6462u -> ExecutionMode.MaximumRegistersIdINTEL(stream.ReadUInt32()) | 6463u -> ExecutionMode.NamedMaximumRegistersINTEL(stream.ReadEnum()) | _ -> failwith "invalid" ))
        | 332us ->
            OpDecorateId(stream.ReadUInt32(), (match stream.ReadUInt32() with | 0u -> Decoration.RelaxedPrecision | 1u -> Decoration.SpecId(stream.ReadUInt32()) | 2u -> Decoration.Block | 3u -> Decoration.BufferBlock | 4u -> Decoration.RowMajor | 5u -> Decoration.ColMajor | 6u -> Decoration.ArrayStride(stream.ReadUInt32()) | 7u -> Decoration.MatrixStride(stream.ReadUInt32()) | 8u -> Decoration.GLSLShared | 9u -> Decoration.GLSLPacked | 10u -> Decoration.CPacked | 11u -> Decoration.BuiltIn(stream.ReadEnum()) | 13u -> Decoration.NoPerspective | 14u -> Decoration.Flat | 15u -> Decoration.Patch | 16u -> Decoration.Centroid | 17u -> Decoration.Sample | 18u -> Decoration.Invariant | 19u -> Decoration.Restrict | 20u -> Decoration.Aliased | 21u -> Decoration.Volatile | 22u -> Decoration.Constant | 23u -> Decoration.Coherent | 24u -> Decoration.NonWritable | 25u -> Decoration.NonReadable | 26u -> Decoration.Uniform | 27u -> Decoration.UniformId(stream.ReadUInt32()) | 28u -> Decoration.SaturatedConversion | 29u -> Decoration.Stream(stream.ReadUInt32()) | 30u -> Decoration.Location(stream.ReadUInt32()) | 31u -> Decoration.Component(stream.ReadUInt32()) | 32u -> Decoration.Index(stream.ReadUInt32()) | 33u -> Decoration.Binding(stream.ReadUInt32()) | 34u -> Decoration.DescriptorSet(stream.ReadUInt32()) | 35u -> Decoration.Offset(stream.ReadUInt32()) | 36u -> Decoration.XfbBuffer(stream.ReadUInt32()) | 37u -> Decoration.XfbStride(stream.ReadUInt32()) | 38u -> Decoration.FuncParamAttr(stream.ReadEnum()) | 39u -> Decoration.FPRoundingMode(stream.ReadEnum()) | 40u -> Decoration.FPFastMathMode(stream.ReadEnum()) | 41u -> Decoration.LinkageAttributes(stream.ReadString(), stream.ReadEnum()) | 42u -> Decoration.NoContraction | 43u -> Decoration.InputAttachmentIndex(stream.ReadUInt32()) | 44u -> Decoration.Alignment(stream.ReadUInt32()) | 45u -> Decoration.MaxByteOffset(stream.ReadUInt32()) | 46u -> Decoration.AlignmentId(stream.ReadUInt32()) | 47u -> Decoration.MaxByteOffsetId(stream.ReadUInt32()) | 4469u -> Decoration.NoSignedWrap | 4470u -> Decoration.NoUnsignedWrap | 4487u -> Decoration.WeightTextureQCOM | 4488u -> Decoration.BlockMatchTextureQCOM | 4499u -> Decoration.BlockMatchSamplerQCOM | 4999u -> Decoration.ExplicitInterpAMD | 5019u -> Decoration.NodeSharesPayloadLimitsWithAMDX(stream.ReadUInt32()) | 5020u -> Decoration.NodeMaxPayloadsAMDX(stream.ReadUInt32()) | 5078u -> Decoration.TrackFinishWritingAMDX | 5091u -> Decoration.PayloadNodeNameAMDX(stream.ReadUInt32()) | 5098u -> Decoration.PayloadNodeBaseIndexAMDX(stream.ReadUInt32()) | 5099u -> Decoration.PayloadNodeSparseArrayAMDX | 5100u -> Decoration.PayloadNodeArraySizeAMDX(stream.ReadUInt32()) | 5105u -> Decoration.PayloadDispatchIndirectAMDX | 5248u -> Decoration.OverrideCoverageNV | 5250u -> Decoration.PassthroughNV | 5252u -> Decoration.ViewportRelativeNV | 5256u -> Decoration.SecondaryViewportRelativeNV(stream.ReadUInt32()) | 5271u -> Decoration.PerPrimitiveEXT | 5272u -> Decoration.PerViewNV | 5273u -> Decoration.PerTaskNV | 5285u -> Decoration.PerVertexKHR | 5300u -> Decoration.NonUniform | 5355u -> Decoration.RestrictPointer | 5356u -> Decoration.AliasedPointer | 5386u -> Decoration.HitObjectShaderRecordBufferNV | 5398u -> Decoration.BindlessSamplerNV | 5399u -> Decoration.BindlessImageNV | 5400u -> Decoration.BoundSamplerNV | 5401u -> Decoration.BoundImageNV | 5599u -> Decoration.SIMTCallINTEL(stream.ReadUInt32()) | 5602u -> Decoration.ReferencedIndirectlyINTEL | 5607u -> Decoration.ClobberINTEL(stream.ReadString()) | 5608u -> Decoration.SideEffectsINTEL | 5624u -> Decoration.VectorComputeVariableINTEL | 5625u -> Decoration.FuncParamIOKindINTEL(stream.ReadUInt32()) | 5626u -> Decoration.VectorComputeFunctionINTEL | 5627u -> Decoration.StackCallINTEL | 5628u -> Decoration.GlobalVariableOffsetINTEL(stream.ReadUInt32()) | 5634u -> Decoration.CounterBuffer(stream.ReadUInt32()) | 5635u -> Decoration.UserSemantic(stream.ReadString()) | 5636u -> Decoration.UserTypeGOOGLE(stream.ReadString()) | 5822u -> Decoration.FunctionRoundingModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 5823u -> Decoration.FunctionDenormModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 5825u -> Decoration.RegisterINTEL | 5826u -> Decoration.MemoryINTEL(stream.ReadString()) | 5827u -> Decoration.NumbanksINTEL(stream.ReadUInt32()) | 5828u -> Decoration.BankwidthINTEL(stream.ReadUInt32()) | 5829u -> Decoration.MaxPrivateCopiesINTEL(stream.ReadUInt32()) | 5830u -> Decoration.SinglepumpINTEL | 5831u -> Decoration.DoublepumpINTEL | 5832u -> Decoration.MaxReplicatesINTEL(stream.ReadUInt32()) | 5833u -> Decoration.SimpleDualPortINTEL | 5834u -> Decoration.MergeINTEL(stream.ReadString(), stream.ReadString()) | 5835u -> Decoration.BankBitsINTEL(stream.ReadUInt32()) | 5836u -> Decoration.ForcePow2DepthINTEL(stream.ReadUInt32()) | 5883u -> Decoration.StridesizeINTEL(stream.ReadUInt32()) | 5884u -> Decoration.WordsizeINTEL(stream.ReadUInt32()) | 5885u -> Decoration.TrueDualPortINTEL | 5899u -> Decoration.BurstCoalesceINTEL | 5900u -> Decoration.CacheSizeINTEL(stream.ReadUInt32()) | 5901u -> Decoration.DontStaticallyCoalesceINTEL | 5902u -> Decoration.PrefetchINTEL(stream.ReadUInt32()) | 5905u -> Decoration.StallEnableINTEL | 5907u -> Decoration.FuseLoopsInFunctionINTEL | 5909u -> Decoration.MathOpDSPModeINTEL(stream.ReadUInt32(), stream.ReadUInt32()) | 5914u -> Decoration.AliasScopeINTEL(stream.ReadUInt32()) | 5915u -> Decoration.NoAliasINTEL(stream.ReadUInt32()) | 5917u -> Decoration.InitiationIntervalINTEL(stream.ReadUInt32()) | 5918u -> Decoration.MaxConcurrencyINTEL(stream.ReadUInt32()) | 5919u -> Decoration.PipelineEnableINTEL(stream.ReadUInt32()) | 5921u -> Decoration.BufferLocationINTEL(stream.ReadUInt32()) | 5944u -> Decoration.IOPipeStorageINTEL(stream.ReadUInt32()) | 6080u -> Decoration.FunctionFloatingPointModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 6085u -> Decoration.SingleElementVectorINTEL | 6087u -> Decoration.VectorComputeCallableFunctionINTEL | 6140u -> Decoration.MediaBlockIOINTEL | 6151u -> Decoration.StallFreeINTEL | 6170u -> Decoration.FPMaxErrorDecorationINTEL(stream.ReadUInt32()) | 6172u -> Decoration.LatencyControlLabelINTEL(stream.ReadUInt32()) | 6173u -> Decoration.LatencyControlConstraintINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 6175u -> Decoration.ConduitKernelArgumentINTEL | 6176u -> Decoration.RegisterMapKernelArgumentINTEL | 6177u -> Decoration.MMHostInterfaceAddressWidthINTEL(stream.ReadUInt32()) | 6178u -> Decoration.MMHostInterfaceDataWidthINTEL(stream.ReadUInt32()) | 6179u -> Decoration.MMHostInterfaceLatencyINTEL(stream.ReadUInt32()) | 6180u -> Decoration.MMHostInterfaceReadWriteModeINTEL(stream.ReadEnum()) | 6181u -> Decoration.MMHostInterfaceMaxBurstINTEL(stream.ReadUInt32()) | 6182u -> Decoration.MMHostInterfaceWaitRequestINTEL(stream.ReadUInt32()) | 6183u -> Decoration.StableKernelArgumentINTEL | 6188u -> Decoration.HostAccessINTEL(stream.ReadEnum(), stream.ReadString()) | 6190u -> Decoration.InitModeINTEL(stream.ReadEnum()) | 6191u -> Decoration.ImplementInRegisterMapINTEL(stream.ReadUInt32()) | 6442u -> Decoration.CacheControlLoadINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 6443u -> Decoration.CacheControlStoreINTEL(stream.ReadUInt32(), stream.ReadEnum()) | _ -> failwith "invalid" ))
        | 333us ->
            OpGroupNonUniformElect(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 334us ->
            OpGroupNonUniformAll(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 335us ->
            OpGroupNonUniformAny(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 336us ->
            OpGroupNonUniformAllEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 337us ->
            OpGroupNonUniformBroadcast(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 338us ->
            OpGroupNonUniformBroadcastFirst(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 339us ->
            OpGroupNonUniformBallot(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 340us ->
            OpGroupNonUniformInverseBallot(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 341us ->
            OpGroupNonUniformBallotBitExtract(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 342us ->
            OpGroupNonUniformBallotBitCount(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 343us ->
            OpGroupNonUniformBallotFindLSB(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 344us ->
            OpGroupNonUniformBallotFindMSB(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 345us ->
            OpGroupNonUniformShuffle(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 346us ->
            OpGroupNonUniformShuffleXor(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 347us ->
            OpGroupNonUniformShuffleUp(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 348us ->
            OpGroupNonUniformShuffleDown(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 349us ->
            OpGroupNonUniformIAdd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 350us ->
            OpGroupNonUniformFAdd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 351us ->
            OpGroupNonUniformIMul(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 352us ->
            OpGroupNonUniformFMul(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 353us ->
            OpGroupNonUniformSMin(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 354us ->
            OpGroupNonUniformUMin(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 355us ->
            OpGroupNonUniformFMin(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 356us ->
            OpGroupNonUniformSMax(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 357us ->
            OpGroupNonUniformUMax(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 358us ->
            OpGroupNonUniformFMax(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 359us ->
            OpGroupNonUniformBitwiseAnd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 360us ->
            OpGroupNonUniformBitwiseOr(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 361us ->
            OpGroupNonUniformBitwiseXor(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 362us ->
            OpGroupNonUniformLogicalAnd(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 363us ->
            OpGroupNonUniformLogicalOr(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 364us ->
            OpGroupNonUniformLogicalXor(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 365us ->
            OpGroupNonUniformQuadBroadcast(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 366us ->
            OpGroupNonUniformQuadSwap(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 400us ->
            OpCopyLogical(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 401us ->
            OpPtrEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 402us ->
            OpPtrNotEqual(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 403us ->
            OpPtrDiff(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4160us ->
            OpColorAttachmentReadEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 4161us ->
            OpDepthAttachmentReadEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 4162us ->
            OpStencilAttachmentReadEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 4416us ->
            OpTerminateInvocation
        | 4417us ->
            OpTypeUntypedPointerKHR(stream.ReadUInt32(), stream.ReadEnum())
        | 4418us ->
            OpUntypedVariableKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadOption(fun () -> stream.ReadUInt32()), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 4419us ->
            OpUntypedAccessChainKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 4420us ->
            OpUntypedInBoundsAccessChainKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 4421us ->
            OpSubgroupBallotKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4422us ->
            OpSubgroupFirstInvocationKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4423us ->
            OpUntypedPtrAccessChainKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 4424us ->
            OpUntypedInBoundsPtrAccessChainKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 4425us ->
            OpUntypedArrayLengthKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4426us ->
            OpUntypedPrefetchKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()), stream.ReadOption(fun () -> stream.ReadUInt32()), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 4428us ->
            OpSubgroupAllKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4429us ->
            OpSubgroupAnyKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4430us ->
            OpSubgroupAllEqualKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4431us ->
            OpGroupNonUniformRotateKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 4432us ->
            OpSubgroupReadInvocationKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4433us ->
            OpExtInstWithForwardRefsKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 4445us ->
            OpTraceRayKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4446us ->
            OpExecuteCallableKHR(stream.ReadUInt32(), stream.ReadUInt32())
        | 4447us ->
            OpConvertUToAccelerationStructureKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4448us ->
            OpIgnoreIntersectionKHR
        | 4449us ->
            OpTerminateRayKHR
        | 4450us ->
            OpSDot(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadEnum()))
        | 4451us ->
            OpUDot(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadEnum()))
        | 4452us ->
            OpSUDot(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadEnum()))
        | 4453us ->
            OpSDotAccSat(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadEnum()))
        | 4454us ->
            OpUDotAccSat(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadEnum()))
        | 4455us ->
            OpSUDotAccSat(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadEnum()))
        | 4456us ->
            OpTypeCooperativeMatrixKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4457us ->
            OpCooperativeMatrixLoadKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 4458us ->
            OpCooperativeMatrixStoreKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 4459us ->
            OpCooperativeMatrixMulAddKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadEnum()))
        | 4460us ->
            OpCooperativeMatrixLengthKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4461us ->
            OpConstantCompositeReplicateEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4462us ->
            OpSpecConstantCompositeReplicateEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4463us ->
            OpCompositeConstructReplicateEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4472us ->
            OpTypeRayQueryKHR(stream.ReadUInt32())
        | 4473us ->
            OpRayQueryInitializeKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4474us ->
            OpRayQueryTerminateKHR(stream.ReadUInt32())
        | 4475us ->
            OpRayQueryGenerateIntersectionKHR(stream.ReadUInt32(), stream.ReadUInt32())
        | 4476us ->
            OpRayQueryConfirmIntersectionKHR(stream.ReadUInt32())
        | 4477us ->
            OpRayQueryProceedKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4479us ->
            OpRayQueryGetIntersectionTypeKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4480us ->
            OpImageSampleWeightedQCOM(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4481us ->
            OpImageBoxFilterQCOM(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4482us ->
            OpImageBlockMatchSSDQCOM(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4483us ->
            OpImageBlockMatchSADQCOM(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4500us ->
            OpImageBlockMatchWindowSSDQCOM(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4501us ->
            OpImageBlockMatchWindowSADQCOM(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4502us ->
            OpImageBlockMatchGatherSSDQCOM(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4503us ->
            OpImageBlockMatchGatherSADQCOM(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5000us ->
            OpGroupIAddNonUniformAMD(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 5001us ->
            OpGroupFAddNonUniformAMD(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 5002us ->
            OpGroupFMinNonUniformAMD(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 5003us ->
            OpGroupUMinNonUniformAMD(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 5004us ->
            OpGroupSMinNonUniformAMD(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 5005us ->
            OpGroupFMaxNonUniformAMD(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 5006us ->
            OpGroupUMaxNonUniformAMD(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 5007us ->
            OpGroupSMaxNonUniformAMD(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 5011us ->
            OpFragmentMaskFetchAMD(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5012us ->
            OpFragmentFetchAMD(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5056us ->
            OpReadClockKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5074us ->
            OpAllocateNodePayloadsAMDX(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5075us ->
            OpEnqueueNodePayloadsAMDX(stream.ReadUInt32())
        | 5076us ->
            OpTypeNodePayloadArrayAMDX(stream.ReadUInt32(), stream.ReadUInt32())
        | 5078us ->
            OpFinishWritingNodePayloadAMDX(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5090us ->
            OpNodePayloadArrayLengthAMDX(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5101us ->
            OpIsNodePayloadValidAMDX(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5103us ->
            OpConstantStringAMDX(stream.ReadUInt32(), stream.ReadString())
        | 5104us ->
            OpSpecConstantStringAMDX(stream.ReadUInt32(), stream.ReadString())
        | 5110us ->
            OpGroupNonUniformQuadAllKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5111us ->
            OpGroupNonUniformQuadAnyKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5249us ->
            OpHitObjectRecordHitMotionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5250us ->
            OpHitObjectRecordHitWithIndexMotionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5251us ->
            OpHitObjectRecordMissMotionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5252us ->
            OpHitObjectGetWorldToObjectNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5253us ->
            OpHitObjectGetObjectToWorldNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5254us ->
            OpHitObjectGetObjectRayDirectionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5255us ->
            OpHitObjectGetObjectRayOriginNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5256us ->
            OpHitObjectTraceRayMotionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5257us ->
            OpHitObjectGetShaderRecordBufferHandleNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5258us ->
            OpHitObjectGetShaderBindingTableRecordIndexNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5259us ->
            OpHitObjectRecordEmptyNV(stream.ReadUInt32())
        | 5260us ->
            OpHitObjectTraceRayNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5261us ->
            OpHitObjectRecordHitNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5262us ->
            OpHitObjectRecordHitWithIndexNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5263us ->
            OpHitObjectRecordMissNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5264us ->
            OpHitObjectExecuteShaderNV(stream.ReadUInt32(), stream.ReadUInt32())
        | 5265us ->
            OpHitObjectGetCurrentTimeNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5266us ->
            OpHitObjectGetAttributesNV(stream.ReadUInt32(), stream.ReadUInt32())
        | 5267us ->
            OpHitObjectGetHitKindNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5268us ->
            OpHitObjectGetPrimitiveIndexNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5269us ->
            OpHitObjectGetGeometryIndexNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5270us ->
            OpHitObjectGetInstanceIdNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5271us ->
            OpHitObjectGetInstanceCustomIndexNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5272us ->
            OpHitObjectGetWorldRayDirectionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5273us ->
            OpHitObjectGetWorldRayOriginNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5274us ->
            OpHitObjectGetRayTMaxNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5275us ->
            OpHitObjectGetRayTMinNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5276us ->
            OpHitObjectIsEmptyNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5277us ->
            OpHitObjectIsHitNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5278us ->
            OpHitObjectIsMissNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5279us ->
            OpReorderThreadWithHitObjectNV(stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 5280us ->
            OpReorderThreadWithHintNV(stream.ReadUInt32(), stream.ReadUInt32())
        | 5281us ->
            OpTypeHitObjectNV(stream.ReadUInt32())
        | 5283us ->
            OpImageSampleFootprintNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | 0x4000u -> ImageOperands.Nontemporal | 0x10000u -> ImageOperands.Offsets(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 5293us ->
            OpCooperativeMatrixConvertNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5294us ->
            OpEmitMeshTasksEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 5295us ->
            OpSetMeshOutputsEXT(stream.ReadUInt32(), stream.ReadUInt32())
        | 5296us ->
            OpGroupNonUniformPartitionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5299us ->
            OpWritePackedPrimitiveIndices4x8NV(stream.ReadUInt32(), stream.ReadUInt32())
        | 5300us ->
            OpFetchMicroTriangleVertexPositionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5301us ->
            OpFetchMicroTriangleVertexBarycentricNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5334us ->
            OpReportIntersectionKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5335us ->
            OpIgnoreIntersectionNV
        | 5336us ->
            OpTerminateRayNV
        | 5337us ->
            OpTraceNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5338us ->
            OpTraceMotionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5339us ->
            OpTraceRayMotionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5340us ->
            OpRayQueryGetIntersectionTriangleVertexPositionsKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5341us ->
            OpTypeAccelerationStructureKHR(stream.ReadUInt32())
        | 5344us ->
            OpExecuteCallableNV(stream.ReadUInt32(), stream.ReadUInt32())
        | 5358us ->
            OpTypeCooperativeMatrixNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5359us ->
            OpCooperativeMatrixLoadNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 5360us ->
            OpCooperativeMatrixStoreNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 5361us ->
            OpCooperativeMatrixMulAddNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5362us ->
            OpCooperativeMatrixLengthNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5364us ->
            OpBeginInvocationInterlockEXT
        | 5365us ->
            OpEndInvocationInterlockEXT
        | 5366us ->
            OpCooperativeMatrixReduceNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 5367us ->
            OpCooperativeMatrixLoadTensorNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" ), (match stream.ReadUInt32() with | 0x0000u -> TensorAddressingOperands.None | 0x0001u -> TensorAddressingOperands.TensorView(stream.ReadUInt32()) | 0x0002u -> TensorAddressingOperands.DecodeFunc(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 5368us ->
            OpCooperativeMatrixStoreTensorNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" ), (match stream.ReadUInt32() with | 0x0000u -> TensorAddressingOperands.None | 0x0001u -> TensorAddressingOperands.TensorView(stream.ReadUInt32()) | 0x0002u -> TensorAddressingOperands.DecodeFunc(stream.ReadUInt32()) | _ -> failwith "invalid" ))
        | 5369us ->
            OpCooperativeMatrixPerElementOpNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5370us ->
            OpTypeTensorLayoutNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5371us ->
            OpTypeTensorViewNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5372us ->
            OpCreateTensorLayoutNV(stream.ReadUInt32(), stream.ReadUInt32())
        | 5373us ->
            OpTensorLayoutSetDimensionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5374us ->
            OpTensorLayoutSetStrideNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5375us ->
            OpTensorLayoutSliceNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5376us ->
            OpTensorLayoutSetClampValueNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5377us ->
            OpCreateTensorViewNV(stream.ReadUInt32(), stream.ReadUInt32())
        | 5378us ->
            OpTensorViewSetDimensionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5379us ->
            OpTensorViewSetStrideNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5380us ->
            OpDemoteToHelperInvocation
        | 5381us ->
            OpIsHelperInvocationEXT(stream.ReadUInt32(), stream.ReadUInt32())
        | 5382us ->
            OpTensorViewSetClipNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5384us ->
            OpTensorLayoutSetBlockSizeNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5390us ->
            OpCooperativeMatrixTransposeNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5391us ->
            OpConvertUToImageNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5392us ->
            OpConvertUToSamplerNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5393us ->
            OpConvertImageToUNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5394us ->
            OpConvertSamplerToUNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5395us ->
            OpConvertUToSampledImageNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5396us ->
            OpConvertSampledImageToUNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5397us ->
            OpSamplerImageAddressingModeNV(stream.ReadUInt32())
        | 5398us ->
            OpRawAccessChainNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadEnum()))
        | 5571us ->
            OpSubgroupShuffleINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5572us ->
            OpSubgroupShuffleDownINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5573us ->
            OpSubgroupShuffleUpINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5574us ->
            OpSubgroupShuffleXorINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5575us ->
            OpSubgroupBlockReadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5576us ->
            OpSubgroupBlockWriteINTEL(stream.ReadUInt32(), stream.ReadUInt32())
        | 5577us ->
            OpSubgroupImageBlockReadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5578us ->
            OpSubgroupImageBlockWriteINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5580us ->
            OpSubgroupImageMediaBlockReadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5581us ->
            OpSubgroupImageMediaBlockWriteINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5585us ->
            OpUCountLeadingZerosINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5586us ->
            OpUCountTrailingZerosINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5587us ->
            OpAbsISubINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5588us ->
            OpAbsUSubINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5589us ->
            OpIAddSatINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5590us ->
            OpUAddSatINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5591us ->
            OpIAverageINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5592us ->
            OpUAverageINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5593us ->
            OpIAverageRoundedINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5594us ->
            OpUAverageRoundedINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5595us ->
            OpISubSatINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5596us ->
            OpUSubSatINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5597us ->
            OpIMul32x16INTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5598us ->
            OpUMul32x16INTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5600us ->
            OpConstantFunctionPointerINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5601us ->
            OpFunctionPointerCallINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5609us ->
            OpAsmTargetINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadString())
        | 5610us ->
            OpAsmINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadString(), stream.ReadString())
        | 5611us ->
            OpAsmCallINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5614us ->
            OpAtomicFMinEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5615us ->
            OpAtomicFMaxEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5630us ->
            OpAssumeTrueKHR(stream.ReadUInt32())
        | 5631us ->
            OpExpectKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5632us ->
            OpDecorateString(stream.ReadUInt32(), (match stream.ReadUInt32() with | 0u -> Decoration.RelaxedPrecision | 1u -> Decoration.SpecId(stream.ReadUInt32()) | 2u -> Decoration.Block | 3u -> Decoration.BufferBlock | 4u -> Decoration.RowMajor | 5u -> Decoration.ColMajor | 6u -> Decoration.ArrayStride(stream.ReadUInt32()) | 7u -> Decoration.MatrixStride(stream.ReadUInt32()) | 8u -> Decoration.GLSLShared | 9u -> Decoration.GLSLPacked | 10u -> Decoration.CPacked | 11u -> Decoration.BuiltIn(stream.ReadEnum()) | 13u -> Decoration.NoPerspective | 14u -> Decoration.Flat | 15u -> Decoration.Patch | 16u -> Decoration.Centroid | 17u -> Decoration.Sample | 18u -> Decoration.Invariant | 19u -> Decoration.Restrict | 20u -> Decoration.Aliased | 21u -> Decoration.Volatile | 22u -> Decoration.Constant | 23u -> Decoration.Coherent | 24u -> Decoration.NonWritable | 25u -> Decoration.NonReadable | 26u -> Decoration.Uniform | 27u -> Decoration.UniformId(stream.ReadUInt32()) | 28u -> Decoration.SaturatedConversion | 29u -> Decoration.Stream(stream.ReadUInt32()) | 30u -> Decoration.Location(stream.ReadUInt32()) | 31u -> Decoration.Component(stream.ReadUInt32()) | 32u -> Decoration.Index(stream.ReadUInt32()) | 33u -> Decoration.Binding(stream.ReadUInt32()) | 34u -> Decoration.DescriptorSet(stream.ReadUInt32()) | 35u -> Decoration.Offset(stream.ReadUInt32()) | 36u -> Decoration.XfbBuffer(stream.ReadUInt32()) | 37u -> Decoration.XfbStride(stream.ReadUInt32()) | 38u -> Decoration.FuncParamAttr(stream.ReadEnum()) | 39u -> Decoration.FPRoundingMode(stream.ReadEnum()) | 40u -> Decoration.FPFastMathMode(stream.ReadEnum()) | 41u -> Decoration.LinkageAttributes(stream.ReadString(), stream.ReadEnum()) | 42u -> Decoration.NoContraction | 43u -> Decoration.InputAttachmentIndex(stream.ReadUInt32()) | 44u -> Decoration.Alignment(stream.ReadUInt32()) | 45u -> Decoration.MaxByteOffset(stream.ReadUInt32()) | 46u -> Decoration.AlignmentId(stream.ReadUInt32()) | 47u -> Decoration.MaxByteOffsetId(stream.ReadUInt32()) | 4469u -> Decoration.NoSignedWrap | 4470u -> Decoration.NoUnsignedWrap | 4487u -> Decoration.WeightTextureQCOM | 4488u -> Decoration.BlockMatchTextureQCOM | 4499u -> Decoration.BlockMatchSamplerQCOM | 4999u -> Decoration.ExplicitInterpAMD | 5019u -> Decoration.NodeSharesPayloadLimitsWithAMDX(stream.ReadUInt32()) | 5020u -> Decoration.NodeMaxPayloadsAMDX(stream.ReadUInt32()) | 5078u -> Decoration.TrackFinishWritingAMDX | 5091u -> Decoration.PayloadNodeNameAMDX(stream.ReadUInt32()) | 5098u -> Decoration.PayloadNodeBaseIndexAMDX(stream.ReadUInt32()) | 5099u -> Decoration.PayloadNodeSparseArrayAMDX | 5100u -> Decoration.PayloadNodeArraySizeAMDX(stream.ReadUInt32()) | 5105u -> Decoration.PayloadDispatchIndirectAMDX | 5248u -> Decoration.OverrideCoverageNV | 5250u -> Decoration.PassthroughNV | 5252u -> Decoration.ViewportRelativeNV | 5256u -> Decoration.SecondaryViewportRelativeNV(stream.ReadUInt32()) | 5271u -> Decoration.PerPrimitiveEXT | 5272u -> Decoration.PerViewNV | 5273u -> Decoration.PerTaskNV | 5285u -> Decoration.PerVertexKHR | 5300u -> Decoration.NonUniform | 5355u -> Decoration.RestrictPointer | 5356u -> Decoration.AliasedPointer | 5386u -> Decoration.HitObjectShaderRecordBufferNV | 5398u -> Decoration.BindlessSamplerNV | 5399u -> Decoration.BindlessImageNV | 5400u -> Decoration.BoundSamplerNV | 5401u -> Decoration.BoundImageNV | 5599u -> Decoration.SIMTCallINTEL(stream.ReadUInt32()) | 5602u -> Decoration.ReferencedIndirectlyINTEL | 5607u -> Decoration.ClobberINTEL(stream.ReadString()) | 5608u -> Decoration.SideEffectsINTEL | 5624u -> Decoration.VectorComputeVariableINTEL | 5625u -> Decoration.FuncParamIOKindINTEL(stream.ReadUInt32()) | 5626u -> Decoration.VectorComputeFunctionINTEL | 5627u -> Decoration.StackCallINTEL | 5628u -> Decoration.GlobalVariableOffsetINTEL(stream.ReadUInt32()) | 5634u -> Decoration.CounterBuffer(stream.ReadUInt32()) | 5635u -> Decoration.UserSemantic(stream.ReadString()) | 5636u -> Decoration.UserTypeGOOGLE(stream.ReadString()) | 5822u -> Decoration.FunctionRoundingModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 5823u -> Decoration.FunctionDenormModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 5825u -> Decoration.RegisterINTEL | 5826u -> Decoration.MemoryINTEL(stream.ReadString()) | 5827u -> Decoration.NumbanksINTEL(stream.ReadUInt32()) | 5828u -> Decoration.BankwidthINTEL(stream.ReadUInt32()) | 5829u -> Decoration.MaxPrivateCopiesINTEL(stream.ReadUInt32()) | 5830u -> Decoration.SinglepumpINTEL | 5831u -> Decoration.DoublepumpINTEL | 5832u -> Decoration.MaxReplicatesINTEL(stream.ReadUInt32()) | 5833u -> Decoration.SimpleDualPortINTEL | 5834u -> Decoration.MergeINTEL(stream.ReadString(), stream.ReadString()) | 5835u -> Decoration.BankBitsINTEL(stream.ReadUInt32()) | 5836u -> Decoration.ForcePow2DepthINTEL(stream.ReadUInt32()) | 5883u -> Decoration.StridesizeINTEL(stream.ReadUInt32()) | 5884u -> Decoration.WordsizeINTEL(stream.ReadUInt32()) | 5885u -> Decoration.TrueDualPortINTEL | 5899u -> Decoration.BurstCoalesceINTEL | 5900u -> Decoration.CacheSizeINTEL(stream.ReadUInt32()) | 5901u -> Decoration.DontStaticallyCoalesceINTEL | 5902u -> Decoration.PrefetchINTEL(stream.ReadUInt32()) | 5905u -> Decoration.StallEnableINTEL | 5907u -> Decoration.FuseLoopsInFunctionINTEL | 5909u -> Decoration.MathOpDSPModeINTEL(stream.ReadUInt32(), stream.ReadUInt32()) | 5914u -> Decoration.AliasScopeINTEL(stream.ReadUInt32()) | 5915u -> Decoration.NoAliasINTEL(stream.ReadUInt32()) | 5917u -> Decoration.InitiationIntervalINTEL(stream.ReadUInt32()) | 5918u -> Decoration.MaxConcurrencyINTEL(stream.ReadUInt32()) | 5919u -> Decoration.PipelineEnableINTEL(stream.ReadUInt32()) | 5921u -> Decoration.BufferLocationINTEL(stream.ReadUInt32()) | 5944u -> Decoration.IOPipeStorageINTEL(stream.ReadUInt32()) | 6080u -> Decoration.FunctionFloatingPointModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 6085u -> Decoration.SingleElementVectorINTEL | 6087u -> Decoration.VectorComputeCallableFunctionINTEL | 6140u -> Decoration.MediaBlockIOINTEL | 6151u -> Decoration.StallFreeINTEL | 6170u -> Decoration.FPMaxErrorDecorationINTEL(stream.ReadUInt32()) | 6172u -> Decoration.LatencyControlLabelINTEL(stream.ReadUInt32()) | 6173u -> Decoration.LatencyControlConstraintINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 6175u -> Decoration.ConduitKernelArgumentINTEL | 6176u -> Decoration.RegisterMapKernelArgumentINTEL | 6177u -> Decoration.MMHostInterfaceAddressWidthINTEL(stream.ReadUInt32()) | 6178u -> Decoration.MMHostInterfaceDataWidthINTEL(stream.ReadUInt32()) | 6179u -> Decoration.MMHostInterfaceLatencyINTEL(stream.ReadUInt32()) | 6180u -> Decoration.MMHostInterfaceReadWriteModeINTEL(stream.ReadEnum()) | 6181u -> Decoration.MMHostInterfaceMaxBurstINTEL(stream.ReadUInt32()) | 6182u -> Decoration.MMHostInterfaceWaitRequestINTEL(stream.ReadUInt32()) | 6183u -> Decoration.StableKernelArgumentINTEL | 6188u -> Decoration.HostAccessINTEL(stream.ReadEnum(), stream.ReadString()) | 6190u -> Decoration.InitModeINTEL(stream.ReadEnum()) | 6191u -> Decoration.ImplementInRegisterMapINTEL(stream.ReadUInt32()) | 6442u -> Decoration.CacheControlLoadINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 6443u -> Decoration.CacheControlStoreINTEL(stream.ReadUInt32(), stream.ReadEnum()) | _ -> failwith "invalid" ))
        | 5633us ->
            OpMemberDecorateString(stream.ReadUInt32(), stream.ReadUInt32(), (match stream.ReadUInt32() with | 0u -> Decoration.RelaxedPrecision | 1u -> Decoration.SpecId(stream.ReadUInt32()) | 2u -> Decoration.Block | 3u -> Decoration.BufferBlock | 4u -> Decoration.RowMajor | 5u -> Decoration.ColMajor | 6u -> Decoration.ArrayStride(stream.ReadUInt32()) | 7u -> Decoration.MatrixStride(stream.ReadUInt32()) | 8u -> Decoration.GLSLShared | 9u -> Decoration.GLSLPacked | 10u -> Decoration.CPacked | 11u -> Decoration.BuiltIn(stream.ReadEnum()) | 13u -> Decoration.NoPerspective | 14u -> Decoration.Flat | 15u -> Decoration.Patch | 16u -> Decoration.Centroid | 17u -> Decoration.Sample | 18u -> Decoration.Invariant | 19u -> Decoration.Restrict | 20u -> Decoration.Aliased | 21u -> Decoration.Volatile | 22u -> Decoration.Constant | 23u -> Decoration.Coherent | 24u -> Decoration.NonWritable | 25u -> Decoration.NonReadable | 26u -> Decoration.Uniform | 27u -> Decoration.UniformId(stream.ReadUInt32()) | 28u -> Decoration.SaturatedConversion | 29u -> Decoration.Stream(stream.ReadUInt32()) | 30u -> Decoration.Location(stream.ReadUInt32()) | 31u -> Decoration.Component(stream.ReadUInt32()) | 32u -> Decoration.Index(stream.ReadUInt32()) | 33u -> Decoration.Binding(stream.ReadUInt32()) | 34u -> Decoration.DescriptorSet(stream.ReadUInt32()) | 35u -> Decoration.Offset(stream.ReadUInt32()) | 36u -> Decoration.XfbBuffer(stream.ReadUInt32()) | 37u -> Decoration.XfbStride(stream.ReadUInt32()) | 38u -> Decoration.FuncParamAttr(stream.ReadEnum()) | 39u -> Decoration.FPRoundingMode(stream.ReadEnum()) | 40u -> Decoration.FPFastMathMode(stream.ReadEnum()) | 41u -> Decoration.LinkageAttributes(stream.ReadString(), stream.ReadEnum()) | 42u -> Decoration.NoContraction | 43u -> Decoration.InputAttachmentIndex(stream.ReadUInt32()) | 44u -> Decoration.Alignment(stream.ReadUInt32()) | 45u -> Decoration.MaxByteOffset(stream.ReadUInt32()) | 46u -> Decoration.AlignmentId(stream.ReadUInt32()) | 47u -> Decoration.MaxByteOffsetId(stream.ReadUInt32()) | 4469u -> Decoration.NoSignedWrap | 4470u -> Decoration.NoUnsignedWrap | 4487u -> Decoration.WeightTextureQCOM | 4488u -> Decoration.BlockMatchTextureQCOM | 4499u -> Decoration.BlockMatchSamplerQCOM | 4999u -> Decoration.ExplicitInterpAMD | 5019u -> Decoration.NodeSharesPayloadLimitsWithAMDX(stream.ReadUInt32()) | 5020u -> Decoration.NodeMaxPayloadsAMDX(stream.ReadUInt32()) | 5078u -> Decoration.TrackFinishWritingAMDX | 5091u -> Decoration.PayloadNodeNameAMDX(stream.ReadUInt32()) | 5098u -> Decoration.PayloadNodeBaseIndexAMDX(stream.ReadUInt32()) | 5099u -> Decoration.PayloadNodeSparseArrayAMDX | 5100u -> Decoration.PayloadNodeArraySizeAMDX(stream.ReadUInt32()) | 5105u -> Decoration.PayloadDispatchIndirectAMDX | 5248u -> Decoration.OverrideCoverageNV | 5250u -> Decoration.PassthroughNV | 5252u -> Decoration.ViewportRelativeNV | 5256u -> Decoration.SecondaryViewportRelativeNV(stream.ReadUInt32()) | 5271u -> Decoration.PerPrimitiveEXT | 5272u -> Decoration.PerViewNV | 5273u -> Decoration.PerTaskNV | 5285u -> Decoration.PerVertexKHR | 5300u -> Decoration.NonUniform | 5355u -> Decoration.RestrictPointer | 5356u -> Decoration.AliasedPointer | 5386u -> Decoration.HitObjectShaderRecordBufferNV | 5398u -> Decoration.BindlessSamplerNV | 5399u -> Decoration.BindlessImageNV | 5400u -> Decoration.BoundSamplerNV | 5401u -> Decoration.BoundImageNV | 5599u -> Decoration.SIMTCallINTEL(stream.ReadUInt32()) | 5602u -> Decoration.ReferencedIndirectlyINTEL | 5607u -> Decoration.ClobberINTEL(stream.ReadString()) | 5608u -> Decoration.SideEffectsINTEL | 5624u -> Decoration.VectorComputeVariableINTEL | 5625u -> Decoration.FuncParamIOKindINTEL(stream.ReadUInt32()) | 5626u -> Decoration.VectorComputeFunctionINTEL | 5627u -> Decoration.StackCallINTEL | 5628u -> Decoration.GlobalVariableOffsetINTEL(stream.ReadUInt32()) | 5634u -> Decoration.CounterBuffer(stream.ReadUInt32()) | 5635u -> Decoration.UserSemantic(stream.ReadString()) | 5636u -> Decoration.UserTypeGOOGLE(stream.ReadString()) | 5822u -> Decoration.FunctionRoundingModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 5823u -> Decoration.FunctionDenormModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 5825u -> Decoration.RegisterINTEL | 5826u -> Decoration.MemoryINTEL(stream.ReadString()) | 5827u -> Decoration.NumbanksINTEL(stream.ReadUInt32()) | 5828u -> Decoration.BankwidthINTEL(stream.ReadUInt32()) | 5829u -> Decoration.MaxPrivateCopiesINTEL(stream.ReadUInt32()) | 5830u -> Decoration.SinglepumpINTEL | 5831u -> Decoration.DoublepumpINTEL | 5832u -> Decoration.MaxReplicatesINTEL(stream.ReadUInt32()) | 5833u -> Decoration.SimpleDualPortINTEL | 5834u -> Decoration.MergeINTEL(stream.ReadString(), stream.ReadString()) | 5835u -> Decoration.BankBitsINTEL(stream.ReadUInt32()) | 5836u -> Decoration.ForcePow2DepthINTEL(stream.ReadUInt32()) | 5883u -> Decoration.StridesizeINTEL(stream.ReadUInt32()) | 5884u -> Decoration.WordsizeINTEL(stream.ReadUInt32()) | 5885u -> Decoration.TrueDualPortINTEL | 5899u -> Decoration.BurstCoalesceINTEL | 5900u -> Decoration.CacheSizeINTEL(stream.ReadUInt32()) | 5901u -> Decoration.DontStaticallyCoalesceINTEL | 5902u -> Decoration.PrefetchINTEL(stream.ReadUInt32()) | 5905u -> Decoration.StallEnableINTEL | 5907u -> Decoration.FuseLoopsInFunctionINTEL | 5909u -> Decoration.MathOpDSPModeINTEL(stream.ReadUInt32(), stream.ReadUInt32()) | 5914u -> Decoration.AliasScopeINTEL(stream.ReadUInt32()) | 5915u -> Decoration.NoAliasINTEL(stream.ReadUInt32()) | 5917u -> Decoration.InitiationIntervalINTEL(stream.ReadUInt32()) | 5918u -> Decoration.MaxConcurrencyINTEL(stream.ReadUInt32()) | 5919u -> Decoration.PipelineEnableINTEL(stream.ReadUInt32()) | 5921u -> Decoration.BufferLocationINTEL(stream.ReadUInt32()) | 5944u -> Decoration.IOPipeStorageINTEL(stream.ReadUInt32()) | 6080u -> Decoration.FunctionFloatingPointModeINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 6085u -> Decoration.SingleElementVectorINTEL | 6087u -> Decoration.VectorComputeCallableFunctionINTEL | 6140u -> Decoration.MediaBlockIOINTEL | 6151u -> Decoration.StallFreeINTEL | 6170u -> Decoration.FPMaxErrorDecorationINTEL(stream.ReadUInt32()) | 6172u -> Decoration.LatencyControlLabelINTEL(stream.ReadUInt32()) | 6173u -> Decoration.LatencyControlConstraintINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 6175u -> Decoration.ConduitKernelArgumentINTEL | 6176u -> Decoration.RegisterMapKernelArgumentINTEL | 6177u -> Decoration.MMHostInterfaceAddressWidthINTEL(stream.ReadUInt32()) | 6178u -> Decoration.MMHostInterfaceDataWidthINTEL(stream.ReadUInt32()) | 6179u -> Decoration.MMHostInterfaceLatencyINTEL(stream.ReadUInt32()) | 6180u -> Decoration.MMHostInterfaceReadWriteModeINTEL(stream.ReadEnum()) | 6181u -> Decoration.MMHostInterfaceMaxBurstINTEL(stream.ReadUInt32()) | 6182u -> Decoration.MMHostInterfaceWaitRequestINTEL(stream.ReadUInt32()) | 6183u -> Decoration.StableKernelArgumentINTEL | 6188u -> Decoration.HostAccessINTEL(stream.ReadEnum(), stream.ReadString()) | 6190u -> Decoration.InitModeINTEL(stream.ReadEnum()) | 6191u -> Decoration.ImplementInRegisterMapINTEL(stream.ReadUInt32()) | 6442u -> Decoration.CacheControlLoadINTEL(stream.ReadUInt32(), stream.ReadEnum()) | 6443u -> Decoration.CacheControlStoreINTEL(stream.ReadUInt32(), stream.ReadEnum()) | _ -> failwith "invalid" ))
        | 5699us ->
            OpVmeImageINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5700us ->
            OpTypeVmeImageINTEL(stream.ReadUInt32(), stream.ReadUInt32())
        | 5701us ->
            OpTypeAvcImePayloadINTEL(stream.ReadUInt32())
        | 5702us ->
            OpTypeAvcRefPayloadINTEL(stream.ReadUInt32())
        | 5703us ->
            OpTypeAvcSicPayloadINTEL(stream.ReadUInt32())
        | 5704us ->
            OpTypeAvcMcePayloadINTEL(stream.ReadUInt32())
        | 5705us ->
            OpTypeAvcMceResultINTEL(stream.ReadUInt32())
        | 5706us ->
            OpTypeAvcImeResultINTEL(stream.ReadUInt32())
        | 5707us ->
            OpTypeAvcImeResultSingleReferenceStreamoutINTEL(stream.ReadUInt32())
        | 5708us ->
            OpTypeAvcImeResultDualReferenceStreamoutINTEL(stream.ReadUInt32())
        | 5709us ->
            OpTypeAvcImeSingleReferenceStreaminINTEL(stream.ReadUInt32())
        | 5710us ->
            OpTypeAvcImeDualReferenceStreaminINTEL(stream.ReadUInt32())
        | 5711us ->
            OpTypeAvcRefResultINTEL(stream.ReadUInt32())
        | 5712us ->
            OpTypeAvcSicResultINTEL(stream.ReadUInt32())
        | 5713us ->
            OpSubgroupAvcMceGetDefaultInterBaseMultiReferencePenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5714us ->
            OpSubgroupAvcMceSetInterBaseMultiReferencePenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5715us ->
            OpSubgroupAvcMceGetDefaultInterShapePenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5716us ->
            OpSubgroupAvcMceSetInterShapePenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5717us ->
            OpSubgroupAvcMceGetDefaultInterDirectionPenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5718us ->
            OpSubgroupAvcMceSetInterDirectionPenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5719us ->
            OpSubgroupAvcMceGetDefaultIntraLumaShapePenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5720us ->
            OpSubgroupAvcMceGetDefaultInterMotionVectorCostTableINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5721us ->
            OpSubgroupAvcMceGetDefaultHighPenaltyCostTableINTEL(stream.ReadUInt32(), stream.ReadUInt32())
        | 5722us ->
            OpSubgroupAvcMceGetDefaultMediumPenaltyCostTableINTEL(stream.ReadUInt32(), stream.ReadUInt32())
        | 5723us ->
            OpSubgroupAvcMceGetDefaultLowPenaltyCostTableINTEL(stream.ReadUInt32(), stream.ReadUInt32())
        | 5724us ->
            OpSubgroupAvcMceSetMotionVectorCostFunctionINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5725us ->
            OpSubgroupAvcMceGetDefaultIntraLumaModePenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5726us ->
            OpSubgroupAvcMceGetDefaultNonDcLumaIntraPenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32())
        | 5727us ->
            OpSubgroupAvcMceGetDefaultIntraChromaModeBasePenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32())
        | 5728us ->
            OpSubgroupAvcMceSetAcOnlyHaarINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5729us ->
            OpSubgroupAvcMceSetSourceInterlacedFieldPolarityINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5730us ->
            OpSubgroupAvcMceSetSingleReferenceInterlacedFieldPolarityINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5731us ->
            OpSubgroupAvcMceSetDualReferenceInterlacedFieldPolaritiesINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5732us ->
            OpSubgroupAvcMceConvertToImePayloadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5733us ->
            OpSubgroupAvcMceConvertToImeResultINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5734us ->
            OpSubgroupAvcMceConvertToRefPayloadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5735us ->
            OpSubgroupAvcMceConvertToRefResultINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5736us ->
            OpSubgroupAvcMceConvertToSicPayloadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5737us ->
            OpSubgroupAvcMceConvertToSicResultINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5738us ->
            OpSubgroupAvcMceGetMotionVectorsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5739us ->
            OpSubgroupAvcMceGetInterDistortionsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5740us ->
            OpSubgroupAvcMceGetBestInterDistortionsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5741us ->
            OpSubgroupAvcMceGetInterMajorShapeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5742us ->
            OpSubgroupAvcMceGetInterMinorShapeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5743us ->
            OpSubgroupAvcMceGetInterDirectionsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5744us ->
            OpSubgroupAvcMceGetInterMotionVectorCountINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5745us ->
            OpSubgroupAvcMceGetInterReferenceIdsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5746us ->
            OpSubgroupAvcMceGetInterReferenceInterlacedFieldPolaritiesINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5747us ->
            OpSubgroupAvcImeInitializeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5748us ->
            OpSubgroupAvcImeSetSingleReferenceINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5749us ->
            OpSubgroupAvcImeSetDualReferenceINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5750us ->
            OpSubgroupAvcImeRefWindowSizeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5751us ->
            OpSubgroupAvcImeAdjustRefOffsetINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5752us ->
            OpSubgroupAvcImeConvertToMcePayloadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5753us ->
            OpSubgroupAvcImeSetMaxMotionVectorCountINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5754us ->
            OpSubgroupAvcImeSetUnidirectionalMixDisableINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5755us ->
            OpSubgroupAvcImeSetEarlySearchTerminationThresholdINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5756us ->
            OpSubgroupAvcImeSetWeightedSadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5757us ->
            OpSubgroupAvcImeEvaluateWithSingleReferenceINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5758us ->
            OpSubgroupAvcImeEvaluateWithDualReferenceINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5759us ->
            OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5760us ->
            OpSubgroupAvcImeEvaluateWithDualReferenceStreaminINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5761us ->
            OpSubgroupAvcImeEvaluateWithSingleReferenceStreamoutINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5762us ->
            OpSubgroupAvcImeEvaluateWithDualReferenceStreamoutINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5763us ->
            OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminoutINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5764us ->
            OpSubgroupAvcImeEvaluateWithDualReferenceStreaminoutINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5765us ->
            OpSubgroupAvcImeConvertToMceResultINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5766us ->
            OpSubgroupAvcImeGetSingleReferenceStreaminINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5767us ->
            OpSubgroupAvcImeGetDualReferenceStreaminINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5768us ->
            OpSubgroupAvcImeStripSingleReferenceStreamoutINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5769us ->
            OpSubgroupAvcImeStripDualReferenceStreamoutINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5770us ->
            OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeMotionVectorsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5771us ->
            OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeDistortionsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5772us ->
            OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeReferenceIdsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5773us ->
            OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeMotionVectorsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5774us ->
            OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeDistortionsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5775us ->
            OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeReferenceIdsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5776us ->
            OpSubgroupAvcImeGetBorderReachedINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5777us ->
            OpSubgroupAvcImeGetTruncatedSearchIndicationINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5778us ->
            OpSubgroupAvcImeGetUnidirectionalEarlySearchTerminationINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5779us ->
            OpSubgroupAvcImeGetWeightingPatternMinimumMotionVectorINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5780us ->
            OpSubgroupAvcImeGetWeightingPatternMinimumDistortionINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5781us ->
            OpSubgroupAvcFmeInitializeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5782us ->
            OpSubgroupAvcBmeInitializeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5783us ->
            OpSubgroupAvcRefConvertToMcePayloadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5784us ->
            OpSubgroupAvcRefSetBidirectionalMixDisableINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5785us ->
            OpSubgroupAvcRefSetBilinearFilterEnableINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5786us ->
            OpSubgroupAvcRefEvaluateWithSingleReferenceINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5787us ->
            OpSubgroupAvcRefEvaluateWithDualReferenceINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5788us ->
            OpSubgroupAvcRefEvaluateWithMultiReferenceINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5789us ->
            OpSubgroupAvcRefEvaluateWithMultiReferenceInterlacedINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5790us ->
            OpSubgroupAvcRefConvertToMceResultINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5791us ->
            OpSubgroupAvcSicInitializeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5792us ->
            OpSubgroupAvcSicConfigureSkcINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5793us ->
            OpSubgroupAvcSicConfigureIpeLumaINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5794us ->
            OpSubgroupAvcSicConfigureIpeLumaChromaINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5795us ->
            OpSubgroupAvcSicGetMotionVectorMaskINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5796us ->
            OpSubgroupAvcSicConvertToMcePayloadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5797us ->
            OpSubgroupAvcSicSetIntraLumaShapePenaltyINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5798us ->
            OpSubgroupAvcSicSetIntraLumaModeCostFunctionINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5799us ->
            OpSubgroupAvcSicSetIntraChromaModeCostFunctionINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5800us ->
            OpSubgroupAvcSicSetBilinearFilterEnableINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5801us ->
            OpSubgroupAvcSicSetSkcForwardTransformEnableINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5802us ->
            OpSubgroupAvcSicSetBlockBasedRawSkipSadINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5803us ->
            OpSubgroupAvcSicEvaluateIpeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5804us ->
            OpSubgroupAvcSicEvaluateWithSingleReferenceINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5805us ->
            OpSubgroupAvcSicEvaluateWithDualReferenceINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5806us ->
            OpSubgroupAvcSicEvaluateWithMultiReferenceINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5807us ->
            OpSubgroupAvcSicEvaluateWithMultiReferenceInterlacedINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5808us ->
            OpSubgroupAvcSicConvertToMceResultINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5809us ->
            OpSubgroupAvcSicGetIpeLumaShapeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5810us ->
            OpSubgroupAvcSicGetBestIpeLumaDistortionINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5811us ->
            OpSubgroupAvcSicGetBestIpeChromaDistortionINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5812us ->
            OpSubgroupAvcSicGetPackedIpeLumaModesINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5813us ->
            OpSubgroupAvcSicGetIpeChromaModeINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5814us ->
            OpSubgroupAvcSicGetPackedSkcLumaCountThresholdINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5815us ->
            OpSubgroupAvcSicGetPackedSkcLumaSumThresholdINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5816us ->
            OpSubgroupAvcSicGetInterRawSadsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5818us ->
            OpVariableLengthArrayINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5819us ->
            OpSaveMemoryINTEL(stream.ReadUInt32(), stream.ReadUInt32())
        | 5820us ->
            OpRestoreMemoryINTEL(stream.ReadUInt32())
        | 5840us ->
            OpArbitraryFloatSinCosPiINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5841us ->
            OpArbitraryFloatCastINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5842us ->
            OpArbitraryFloatCastFromIntINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5843us ->
            OpArbitraryFloatCastToIntINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5846us ->
            OpArbitraryFloatAddINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5847us ->
            OpArbitraryFloatSubINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5848us ->
            OpArbitraryFloatMulINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5849us ->
            OpArbitraryFloatDivINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5850us ->
            OpArbitraryFloatGTINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5851us ->
            OpArbitraryFloatGEINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5852us ->
            OpArbitraryFloatLTINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5853us ->
            OpArbitraryFloatLEINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5854us ->
            OpArbitraryFloatEQINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5855us ->
            OpArbitraryFloatRecipINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5856us ->
            OpArbitraryFloatRSqrtINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5857us ->
            OpArbitraryFloatCbrtINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5858us ->
            OpArbitraryFloatHypotINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5859us ->
            OpArbitraryFloatSqrtINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5860us ->
            OpArbitraryFloatLogINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5861us ->
            OpArbitraryFloatLog2INTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5862us ->
            OpArbitraryFloatLog10INTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5863us ->
            OpArbitraryFloatLog1pINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5864us ->
            OpArbitraryFloatExpINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5865us ->
            OpArbitraryFloatExp2INTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5866us ->
            OpArbitraryFloatExp10INTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5867us ->
            OpArbitraryFloatExpm1INTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5868us ->
            OpArbitraryFloatSinINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5869us ->
            OpArbitraryFloatCosINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5870us ->
            OpArbitraryFloatSinCosINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5871us ->
            OpArbitraryFloatSinPiINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5872us ->
            OpArbitraryFloatCosPiINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5873us ->
            OpArbitraryFloatASinINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5874us ->
            OpArbitraryFloatASinPiINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5875us ->
            OpArbitraryFloatACosINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5876us ->
            OpArbitraryFloatACosPiINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5877us ->
            OpArbitraryFloatATanINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5878us ->
            OpArbitraryFloatATanPiINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5879us ->
            OpArbitraryFloatATan2INTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5880us ->
            OpArbitraryFloatPowINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5881us ->
            OpArbitraryFloatPowRINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5882us ->
            OpArbitraryFloatPowNINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5887us ->
            OpLoopControlINTEL(stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5911us ->
            OpAliasDomainDeclINTEL(stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 5912us ->
            OpAliasScopeDeclINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> stream.ReadUInt32()))
        | 5913us ->
            OpAliasScopeListDeclINTEL(stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 5923us ->
            OpFixedSqrtINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5924us ->
            OpFixedRecipINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5925us ->
            OpFixedRsqrtINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5926us ->
            OpFixedSinINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5927us ->
            OpFixedCosINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5928us ->
            OpFixedSinCosINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5929us ->
            OpFixedSinPiINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5930us ->
            OpFixedCosPiINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5931us ->
            OpFixedSinCosPiINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5932us ->
            OpFixedLogINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5933us ->
            OpFixedExpINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5934us ->
            OpPtrCastToCrossWorkgroupINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5938us ->
            OpCrossWorkgroupCastToPtrINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5946us ->
            OpReadPipeBlockingINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5947us ->
            OpWritePipeBlockingINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5949us ->
            OpFPGARegINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6016us ->
            OpRayQueryGetRayTMinKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6017us ->
            OpRayQueryGetRayFlagsKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6018us ->
            OpRayQueryGetIntersectionTKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6019us ->
            OpRayQueryGetIntersectionInstanceCustomIndexKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6020us ->
            OpRayQueryGetIntersectionInstanceIdKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6021us ->
            OpRayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6022us ->
            OpRayQueryGetIntersectionGeometryIndexKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6023us ->
            OpRayQueryGetIntersectionPrimitiveIndexKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6024us ->
            OpRayQueryGetIntersectionBarycentricsKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6025us ->
            OpRayQueryGetIntersectionFrontFaceKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6026us ->
            OpRayQueryGetIntersectionCandidateAABBOpaqueKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6027us ->
            OpRayQueryGetIntersectionObjectRayDirectionKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6028us ->
            OpRayQueryGetIntersectionObjectRayOriginKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6029us ->
            OpRayQueryGetWorldRayDirectionKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6030us ->
            OpRayQueryGetWorldRayOriginKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6031us ->
            OpRayQueryGetIntersectionObjectToWorldKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6032us ->
            OpRayQueryGetIntersectionWorldToObjectKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6035us ->
            OpAtomicFAddEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6086us ->
            OpTypeBufferSurfaceINTEL(stream.ReadUInt32(), stream.ReadEnum())
        | 6090us ->
            OpTypeStructContinuedINTEL(stream.ReadList(fun () -> stream.ReadUInt32()))
        | 6091us ->
            OpConstantCompositeContinuedINTEL(stream.ReadList(fun () -> stream.ReadUInt32()))
        | 6092us ->
            OpSpecConstantCompositeContinuedINTEL(stream.ReadList(fun () -> stream.ReadUInt32()))
        | 6096us ->
            OpCompositeConstructContinuedINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadList(fun () -> stream.ReadUInt32()))
        | 6116us ->
            OpConvertFToBF16INTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6117us ->
            OpConvertBF16ToFINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6142us ->
            OpControlBarrierArriveINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6143us ->
            OpControlBarrierWaitINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6145us ->
            OpArithmeticFenceEXT(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6221us ->
            OpSubgroupBlockPrefetchINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> (match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | 0x10000u -> MemoryAccess.AliasScopeINTELMask(stream.ReadUInt32()) | 0x20000u -> MemoryAccess.NoAliasINTELMask(stream.ReadUInt32()) | _ -> failwith "invalid" )))
        | 6401us ->
            OpGroupIMulKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 6402us ->
            OpGroupFMulKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 6403us ->
            OpGroupBitwiseAndKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 6404us ->
            OpGroupBitwiseOrKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 6405us ->
            OpGroupBitwiseXorKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 6406us ->
            OpGroupLogicalAndKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 6407us ->
            OpGroupLogicalOrKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 6408us ->
            OpGroupLogicalXorKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadEnum(), stream.ReadUInt32())
        | 6428us ->
            OpMaskedGatherINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 6429us ->
            OpMaskedScatterINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())        | _ -> failwith "invalid opcode" 