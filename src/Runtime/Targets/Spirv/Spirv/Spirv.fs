// File is generated. Do not modify.
[<AutoOpen>]
module rec Spirv.Core

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

type FPFastMathMode =
   | None = 0x0000u
   | NotNaN = 0x0001u
   | NotInf = 0x0002u
   | NSZ = 0x0004u
   | AllowRecip = 0x0008u
   | Fast = 0x0010u

type SelectionControl =
   | None = 0x0000u
   | Flatten = 0x0001u
   | DontFlatten = 0x0002u

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

type FunctionControl =
   | None = 0x0000u
   | Inline = 0x0001u
   | DontInline = 0x0002u
   | Pure = 0x0004u
   | Const = 0x0008u

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

[<RequireQualifiedAccess>]
type MemoryAccess =
    | None
    | Volatile
    | Aligned of LiteralInteger
    | Nontemporal
    | MakePointerAvailable of IdScope
    | MakePointerVisible of IdScope
    | NonPrivatePointer

    member x.Value =
       match x with
       | None -> 0x0000u
       | Volatile -> 0x0001u
       | Aligned _ -> 0x0002u
       | Nontemporal -> 0x0004u
       | MakePointerAvailable _ -> 0x0008u
       | MakePointerVisible _ -> 0x0010u
       | NonPrivatePointer -> 0x0020u

type KernelProfilingInfo =
   | None = 0x0000u
   | CmdExecTime = 0x0001u

type SourceLanguage =
   | Unknown = 0u
   | ESSL = 1u
   | GLSL = 2u
   | OpenCL_C = 3u
   | OpenCL_CPP = 4u
   | HLSL = 5u

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
   | RayGenerationNV = 5313u
   | IntersectionNV = 5314u
   | AnyHitNV = 5315u
   | ClosestHitNV = 5316u
   | MissNV = 5317u
   | CallableNV = 5318u

type AddressingModel =
   | Logical = 0u
   | Physical32 = 1u
   | Physical64 = 2u
   | PhysicalStorageBuffer64 = 5348u

type MemoryModel =
   | Simple = 0u
   | GLSL450 = 1u
   | OpenCL = 2u
   | Vulkan = 3u

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
    | LocalSizeHintId of LocalSizeHint: IdRef
    | PostDepthCoverage
    | DenormPreserve of TargetWidth: LiteralInteger
    | DenormFlushToZero of TargetWidth: LiteralInteger
    | SignedZeroInfNanPreserve of TargetWidth: LiteralInteger
    | RoundingModeRTE of TargetWidth: LiteralInteger
    | RoundingModeRTZ of TargetWidth: LiteralInteger
    | StencilRefReplacingEXT
    | OutputLinesNV
    | OutputPrimitivesNV of Primitivecount: LiteralInteger
    | DerivativeGroupQuadsNV
    | DerivativeGroupLinearNV
    | OutputTrianglesNV
    | PixelInterlockOrderedEXT
    | PixelInterlockUnorderedEXT
    | SampleInterlockOrderedEXT
    | SampleInterlockUnorderedEXT
    | ShadingRateInterlockOrderedEXT
    | ShadingRateInterlockUnorderedEXT

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
       | PostDepthCoverage -> 4446u
       | DenormPreserve _ -> 4459u
       | DenormFlushToZero _ -> 4460u
       | SignedZeroInfNanPreserve _ -> 4461u
       | RoundingModeRTE _ -> 4462u
       | RoundingModeRTZ _ -> 4463u
       | StencilRefReplacingEXT -> 5027u
       | OutputLinesNV -> 5269u
       | OutputPrimitivesNV _ -> 5270u
       | DerivativeGroupQuadsNV -> 5289u
       | DerivativeGroupLinearNV -> 5290u
       | OutputTrianglesNV -> 5298u
       | PixelInterlockOrderedEXT -> 5366u
       | PixelInterlockUnorderedEXT -> 5367u
       | SampleInterlockOrderedEXT -> 5368u
       | SampleInterlockUnorderedEXT -> 5369u
       | ShadingRateInterlockOrderedEXT -> 5370u
       | ShadingRateInterlockUnorderedEXT -> 5371u

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
   | CallableDataNV = 5328u
   | IncomingCallableDataNV = 5329u
   | RayPayloadNV = 5338u
   | HitAttributeNV = 5339u
   | IncomingRayPayloadNV = 5342u
   | ShaderRecordBufferNV = 5343u
   | PhysicalStorageBuffer = 5349u

type Dim =
   | One = 0u
   | Two = 1u
   | Three = 2u
   | Cube = 3u
   | Rect = 4u
   | Buffer = 5u
   | SubpassData = 6u

type SamplerAddressingMode =
   | None = 0u
   | ClampToEdge = 1u
   | Clamp = 2u
   | Repeat = 3u
   | RepeatMirrored = 4u

type SamplerFilterMode =
   | Nearest = 0u
   | Linear = 1u

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

type FPRoundingMode =
   | RTE = 0u
   | RTZ = 1u
   | RTP = 2u
   | RTN = 3u

type LinkageType =
   | Export = 0u
   | Import = 1u

type AccessQualifier =
   | ReadOnly = 0u
   | WriteOnly = 1u
   | ReadWrite = 2u

type FunctionParameterAttribute =
   | Zext = 0u
   | Sext = 1u
   | ByVal = 2u
   | Sret = 3u
   | NoAlias = 4u
   | NoCapture = 5u
   | NoWrite = 6u
   | NoReadWrite = 7u

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
    | ExplicitInterpAMD
    | OverrideCoverageNV
    | PassthroughNV
    | ViewportRelativeNV
    | SecondaryViewportRelativeNV of Offset: LiteralInteger
    | PerPrimitiveNV
    | PerViewNV
    | PerTaskNV
    | PerVertexNV
    | NonUniform
    | RestrictPointer
    | AliasedPointer
    | CounterBuffer of CounterBuffer: IdRef
    | UserSemantic of Semantic: LiteralString
    | UserTypeGOOGLE of UserType: LiteralString

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
       | ExplicitInterpAMD -> 4999u
       | OverrideCoverageNV -> 5248u
       | PassthroughNV -> 5250u
       | ViewportRelativeNV -> 5252u
       | SecondaryViewportRelativeNV _ -> 5256u
       | PerPrimitiveNV -> 5271u
       | PerViewNV -> 5272u
       | PerTaskNV -> 5273u
       | PerVertexNV -> 5285u
       | NonUniform -> 5300u
       | RestrictPointer -> 5355u
       | AliasedPointer -> 5356u
       | CounterBuffer _ -> 5634u
       | UserSemantic _ -> 5635u
       | UserTypeGOOGLE _ -> 5636u

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
   | SubgroupEqMask = 4416u
   | SubgroupGeMask = 4417u
   | SubgroupGtMask = 4418u
   | SubgroupLeMask = 4419u
   | SubgroupLtMask = 4420u
   | BaseVertex = 4424u
   | BaseInstance = 4425u
   | DrawIndex = 4426u
   | DeviceIndex = 4438u
   | ViewIndex = 4440u
   | BaryCoordNoPerspAMD = 4992u
   | BaryCoordNoPerspCentroidAMD = 4993u
   | BaryCoordNoPerspSampleAMD = 4994u
   | BaryCoordSmoothAMD = 4995u
   | BaryCoordSmoothCentroidAMD = 4996u
   | BaryCoordSmoothSampleAMD = 4997u
   | BaryCoordPullModelAMD = 4998u
   | FragStencilRefEXT = 5014u
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
   | BaryCoordNV = 5286u
   | BaryCoordNoPerspNV = 5287u
   | FragSizeEXT = 5292u
   | FragInvocationCountEXT = 5293u
   | LaunchIdNV = 5319u
   | LaunchSizeNV = 5320u
   | WorldRayOriginNV = 5321u
   | WorldRayDirectionNV = 5322u
   | ObjectRayOriginNV = 5323u
   | ObjectRayDirectionNV = 5324u
   | RayTminNV = 5325u
   | RayTmaxNV = 5326u
   | InstanceCustomIndexNV = 5327u
   | ObjectToWorldNV = 5330u
   | WorldToObjectNV = 5331u
   | HitTNV = 5332u
   | HitKindNV = 5333u
   | IncomingRayFlagsNV = 5351u
   | WarpsPerSMNV = 5374u
   | SMCountNV = 5375u
   | WarpIDNV = 5376u
   | SMIDNV = 5377u

type Scope =
   | CrossDevice = 0u
   | Device = 1u
   | Workgroup = 2u
   | Subgroup = 3u
   | Invocation = 4u
   | QueueFamily = 5u

type GroupOperation =
   | Reduce = 0u
   | InclusiveScan = 1u
   | ExclusiveScan = 2u
   | ClusteredReduce = 3u
   | PartitionedReduceNV = 6u
   | PartitionedInclusiveScanNV = 7u
   | PartitionedExclusiveScanNV = 8u

type KernelEnqueueFlags =
   | NoWait = 0u
   | WaitKernel = 1u
   | WaitWorkGroup = 2u

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
   | SubgroupBallotKHR = 4423u
   | DrawParameters = 4427u
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
   | Float16ImageAMD = 5008u
   | ImageGatherBiasLodAMD = 5009u
   | FragmentMaskAMD = 5010u
   | StencilExportEXT = 5013u
   | ImageReadWriteLodAMD = 5015u
   | ShaderClockKHR = 5055u
   | SampleMaskOverrideCoverageNV = 5249u
   | GeometryShaderPassthroughNV = 5251u
   | ShaderViewportIndexLayerEXT = 5254u
   | ShaderViewportMaskNV = 5255u
   | ShaderStereoViewNV = 5259u
   | PerViewAttributesNV = 5260u
   | FragmentFullyCoveredEXT = 5265u
   | MeshShadingNV = 5266u
   | ImageFootprintNV = 5282u
   | FragmentBarycentricNV = 5284u
   | ComputeDerivativeGroupQuadsNV = 5288u
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
   | RayTracingNV = 5340u
   | VulkanMemoryModel = 5345u
   | VulkanMemoryModelDeviceScope = 5346u
   | PhysicalStorageBufferAddresses = 5347u
   | ComputeDerivativeGroupLinearNV = 5350u
   | CooperativeMatrixNV = 5357u
   | FragmentShaderSampleInterlockEXT = 5363u
   | FragmentShaderShadingRateInterlockEXT = 5372u
   | ShaderSMBuiltinsNV = 5373u
   | FragmentShaderPixelInterlockEXT = 5378u
   | DemoteToHelperInvocationEXT = 5379u
   | SubgroupShuffleINTEL = 5568u
   | SubgroupBufferBlockIOINTEL = 5569u
   | SubgroupImageBlockIOINTEL = 5570u
   | SubgroupImageMediaBlockIOINTEL = 5579u
   | IntegerFunctions2INTEL = 5584u
   | SubgroupAvcMotionEstimationINTEL = 5696u
   | SubgroupAvcMotionEstimationIntraINTEL = 5697u
   | SubgroupAvcMotionEstimationChromaINTEL = 5698u

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
   | OpTypeFloat of IdResult * Width: LiteralInteger
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
   | OpSubgroupBallotKHR of IdResultType * IdResult * Predicate: IdRef
   | OpSubgroupFirstInvocationKHR of IdResultType * IdResult * Value: IdRef
   | OpSubgroupAllKHR of IdResultType * IdResult * Predicate: IdRef
   | OpSubgroupAnyKHR of IdResultType * IdResult * Predicate: IdRef
   | OpSubgroupAllEqualKHR of IdResultType * IdResult * Predicate: IdRef
   | OpSubgroupReadInvocationKHR of IdResultType * IdResult * Value: IdRef * Index: IdRef
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
   | OpReadClockKHR of IdResultType * IdResult * Execution: IdScope
   | OpImageSampleFootprintNV of IdResultType * IdResult * SampledImage: IdRef * Coordinate: IdRef * Granularity: IdRef * Coarse: IdRef * ImageOperands option
   | OpGroupNonUniformPartitionNV of IdResultType * IdResult * Value: IdRef
   | OpWritePackedPrimitiveIndices4x8NV of IndexOffset: IdRef * PackedIndices: IdRef
   | OpReportIntersectionNV of IdResultType * IdResult * Hit: IdRef * HitKind: IdRef
   | OpIgnoreIntersectionNV
   | OpTerminateRayNV
   | OpTraceNV of Accel: IdRef * RayFlags: IdRef * CullMask: IdRef * SBTOffset: IdRef * SBTStride: IdRef * MissIndex: IdRef * RayOrigin: IdRef * RayTmin: IdRef * RayDirection: IdRef * RayTmax: IdRef * PayloadId: IdRef
   | OpTypeAccelerationStructureNV of IdResult
   | OpExecuteCallableNV of SBTIndex: IdRef * CallableDataId: IdRef
   | OpTypeCooperativeMatrixNV of IdResult * ComponentType: IdRef * Execution: IdScope * Rows: IdRef * Columns: IdRef
   | OpCooperativeMatrixLoadNV of IdResultType * IdResult * Pointer: IdRef * Stride: IdRef * ColumnMajor: IdRef * MemoryAccess option
   | OpCooperativeMatrixStoreNV of Pointer: IdRef * Object: IdRef * Stride: IdRef * ColumnMajor: IdRef * MemoryAccess option
   | OpCooperativeMatrixMulAddNV of IdResultType * IdResult * A: IdRef * B: IdRef * C: IdRef
   | OpCooperativeMatrixLengthNV of IdResultType * IdResult * Type: IdRef
   | OpBeginInvocationInterlockEXT
   | OpEndInvocationInterlockEXT
   | OpDemoteToHelperInvocationEXT
   | OpIsHelperInvocationEXT of IdResultType * IdResult
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
       | OpSubgroupBallotKHR _ -> 4421us
       | OpSubgroupFirstInvocationKHR _ -> 4422us
       | OpSubgroupAllKHR _ -> 4428us
       | OpSubgroupAnyKHR _ -> 4429us
       | OpSubgroupAllEqualKHR _ -> 4430us
       | OpSubgroupReadInvocationKHR _ -> 4432us
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
       | OpImageSampleFootprintNV _ -> 5283us
       | OpGroupNonUniformPartitionNV _ -> 5296us
       | OpWritePackedPrimitiveIndices4x8NV _ -> 5299us
       | OpReportIntersectionNV _ -> 5334us
       | OpIgnoreIntersectionNV -> 5335us
       | OpTerminateRayNV -> 5336us
       | OpTraceNV _ -> 5337us
       | OpTypeAccelerationStructureNV _ -> 5341us
       | OpExecuteCallableNV _ -> 5344us
       | OpTypeCooperativeMatrixNV _ -> 5358us
       | OpCooperativeMatrixLoadNV _ -> 5359us
       | OpCooperativeMatrixStoreNV _ -> 5360us
       | OpCooperativeMatrixMulAddNV _ -> 5361us
       | OpCooperativeMatrixLengthNV _ -> 5362us
       | OpBeginInvocationInterlockEXT -> 5364us
       | OpEndInvocationInterlockEXT -> 5365us
       | OpDemoteToHelperInvocationEXT -> 5380us
       | OpIsHelperInvocationEXT _ -> 5381us
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

    member x.Version =
       match x with
       | OpNop -> 1.0m
       | OpUndef _ -> 1.0m
       | OpSourceContinued _ -> 1.0m
       | OpSource _ -> 1.0m
       | OpSourceExtension _ -> 1.0m
       | OpName _ -> 1.0m
       | OpMemberName _ -> 1.0m
       | OpString _ -> 1.0m
       | OpLine _ -> 1.0m
       | OpExtension _ -> 1.0m
       | OpExtInstImport _ -> 1.0m
       | OpExtInst _ -> 1.0m
       | OpMemoryModel _ -> 1.0m
       | OpEntryPoint _ -> 1.0m
       | OpExecutionMode _ -> 1.0m
       | OpCapability _ -> 1.0m
       | OpTypeVoid _ -> 1.0m
       | OpTypeBool _ -> 1.0m
       | OpTypeInt _ -> 1.0m
       | OpTypeFloat _ -> 1.0m
       | OpTypeVector _ -> 1.0m
       | OpTypeMatrix _ -> 1.0m
       | OpTypeImage _ -> 1.0m
       | OpTypeSampler _ -> 1.0m
       | OpTypeSampledImage _ -> 1.0m
       | OpTypeArray _ -> 1.0m
       | OpTypeRuntimeArray _ -> 1.0m
       | OpTypeStruct _ -> 1.0m
       | OpTypeOpaque _ -> 1.0m
       | OpTypePointer _ -> 1.0m
       | OpTypeFunction _ -> 1.0m
       | OpTypeEvent _ -> 1.0m
       | OpTypeDeviceEvent _ -> 1.0m
       | OpTypeReserveId _ -> 1.0m
       | OpTypeQueue _ -> 1.0m
       | OpTypePipe _ -> 1.0m
       | OpTypeForwardPointer _ -> 1.0m
       | OpConstantTrue _ -> 1.0m
       | OpConstantFalse _ -> 1.0m
       | OpConstant _ -> 1.0m
       | OpConstantComposite _ -> 1.0m
       | OpConstantSampler _ -> 1.0m
       | OpConstantNull _ -> 1.0m
       | OpSpecConstantTrue _ -> 1.0m
       | OpSpecConstantFalse _ -> 1.0m
       | OpSpecConstant _ -> 1.0m
       | OpSpecConstantComposite _ -> 1.0m
       | OpSpecConstantOp _ -> 1.0m
       | OpFunction _ -> 1.0m
       | OpFunctionParameter _ -> 1.0m
       | OpFunctionEnd -> 1.0m
       | OpFunctionCall _ -> 1.0m
       | OpVariable _ -> 1.0m
       | OpImageTexelPointer _ -> 1.0m
       | OpLoad _ -> 1.0m
       | OpStore _ -> 1.0m
       | OpCopyMemory _ -> 1.0m
       | OpCopyMemorySized _ -> 1.0m
       | OpAccessChain _ -> 1.0m
       | OpInBoundsAccessChain _ -> 1.0m
       | OpPtrAccessChain _ -> 1.0m
       | OpArrayLength _ -> 1.0m
       | OpGenericPtrMemSemantics _ -> 1.0m
       | OpInBoundsPtrAccessChain _ -> 1.0m
       | OpDecorate _ -> 1.0m
       | OpMemberDecorate _ -> 1.0m
       | OpDecorationGroup _ -> 1.0m
       | OpGroupDecorate _ -> 1.0m
       | OpGroupMemberDecorate _ -> 1.0m
       | OpVectorExtractDynamic _ -> 1.0m
       | OpVectorInsertDynamic _ -> 1.0m
       | OpVectorShuffle _ -> 1.0m
       | OpCompositeConstruct _ -> 1.0m
       | OpCompositeExtract _ -> 1.0m
       | OpCompositeInsert _ -> 1.0m
       | OpCopyObject _ -> 1.0m
       | OpTranspose _ -> 1.0m
       | OpSampledImage _ -> 1.0m
       | OpImageSampleImplicitLod _ -> 1.0m
       | OpImageSampleExplicitLod _ -> 1.0m
       | OpImageSampleDrefImplicitLod _ -> 1.0m
       | OpImageSampleDrefExplicitLod _ -> 1.0m
       | OpImageSampleProjImplicitLod _ -> 1.0m
       | OpImageSampleProjExplicitLod _ -> 1.0m
       | OpImageSampleProjDrefImplicitLod _ -> 1.0m
       | OpImageSampleProjDrefExplicitLod _ -> 1.0m
       | OpImageFetch _ -> 1.0m
       | OpImageGather _ -> 1.0m
       | OpImageDrefGather _ -> 1.0m
       | OpImageRead _ -> 1.0m
       | OpImageWrite _ -> 1.0m
       | OpImage _ -> 1.0m
       | OpImageQueryFormat _ -> 1.0m
       | OpImageQueryOrder _ -> 1.0m
       | OpImageQuerySizeLod _ -> 1.0m
       | OpImageQuerySize _ -> 1.0m
       | OpImageQueryLod _ -> 1.0m
       | OpImageQueryLevels _ -> 1.0m
       | OpImageQuerySamples _ -> 1.0m
       | OpConvertFToU _ -> 1.0m
       | OpConvertFToS _ -> 1.0m
       | OpConvertSToF _ -> 1.0m
       | OpConvertUToF _ -> 1.0m
       | OpUConvert _ -> 1.0m
       | OpSConvert _ -> 1.0m
       | OpFConvert _ -> 1.0m
       | OpQuantizeToF16 _ -> 1.0m
       | OpConvertPtrToU _ -> 1.0m
       | OpSatConvertSToU _ -> 1.0m
       | OpSatConvertUToS _ -> 1.0m
       | OpConvertUToPtr _ -> 1.0m
       | OpPtrCastToGeneric _ -> 1.0m
       | OpGenericCastToPtr _ -> 1.0m
       | OpGenericCastToPtrExplicit _ -> 1.0m
       | OpBitcast _ -> 1.0m
       | OpSNegate _ -> 1.0m
       | OpFNegate _ -> 1.0m
       | OpIAdd _ -> 1.0m
       | OpFAdd _ -> 1.0m
       | OpISub _ -> 1.0m
       | OpFSub _ -> 1.0m
       | OpIMul _ -> 1.0m
       | OpFMul _ -> 1.0m
       | OpUDiv _ -> 1.0m
       | OpSDiv _ -> 1.0m
       | OpFDiv _ -> 1.0m
       | OpUMod _ -> 1.0m
       | OpSRem _ -> 1.0m
       | OpSMod _ -> 1.0m
       | OpFRem _ -> 1.0m
       | OpFMod _ -> 1.0m
       | OpVectorTimesScalar _ -> 1.0m
       | OpMatrixTimesScalar _ -> 1.0m
       | OpVectorTimesMatrix _ -> 1.0m
       | OpMatrixTimesVector _ -> 1.0m
       | OpMatrixTimesMatrix _ -> 1.0m
       | OpOuterProduct _ -> 1.0m
       | OpDot _ -> 1.0m
       | OpIAddCarry _ -> 1.0m
       | OpISubBorrow _ -> 1.0m
       | OpUMulExtended _ -> 1.0m
       | OpSMulExtended _ -> 1.0m
       | OpAny _ -> 1.0m
       | OpAll _ -> 1.0m
       | OpIsNan _ -> 1.0m
       | OpIsInf _ -> 1.0m
       | OpIsFinite _ -> 1.0m
       | OpIsNormal _ -> 1.0m
       | OpSignBitSet _ -> 1.0m
       | OpLessOrGreater _ -> 1.0m
       | OpOrdered _ -> 1.0m
       | OpUnordered _ -> 1.0m
       | OpLogicalEqual _ -> 1.0m
       | OpLogicalNotEqual _ -> 1.0m
       | OpLogicalOr _ -> 1.0m
       | OpLogicalAnd _ -> 1.0m
       | OpLogicalNot _ -> 1.0m
       | OpSelect _ -> 1.0m
       | OpIEqual _ -> 1.0m
       | OpINotEqual _ -> 1.0m
       | OpUGreaterThan _ -> 1.0m
       | OpSGreaterThan _ -> 1.0m
       | OpUGreaterThanEqual _ -> 1.0m
       | OpSGreaterThanEqual _ -> 1.0m
       | OpULessThan _ -> 1.0m
       | OpSLessThan _ -> 1.0m
       | OpULessThanEqual _ -> 1.0m
       | OpSLessThanEqual _ -> 1.0m
       | OpFOrdEqual _ -> 1.0m
       | OpFUnordEqual _ -> 1.0m
       | OpFOrdNotEqual _ -> 1.0m
       | OpFUnordNotEqual _ -> 1.0m
       | OpFOrdLessThan _ -> 1.0m
       | OpFUnordLessThan _ -> 1.0m
       | OpFOrdGreaterThan _ -> 1.0m
       | OpFUnordGreaterThan _ -> 1.0m
       | OpFOrdLessThanEqual _ -> 1.0m
       | OpFUnordLessThanEqual _ -> 1.0m
       | OpFOrdGreaterThanEqual _ -> 1.0m
       | OpFUnordGreaterThanEqual _ -> 1.0m
       | OpShiftRightLogical _ -> 1.0m
       | OpShiftRightArithmetic _ -> 1.0m
       | OpShiftLeftLogical _ -> 1.0m
       | OpBitwiseOr _ -> 1.0m
       | OpBitwiseXor _ -> 1.0m
       | OpBitwiseAnd _ -> 1.0m
       | OpNot _ -> 1.0m
       | OpBitFieldInsert _ -> 1.0m
       | OpBitFieldSExtract _ -> 1.0m
       | OpBitFieldUExtract _ -> 1.0m
       | OpBitReverse _ -> 1.0m
       | OpBitCount _ -> 1.0m
       | OpDPdx _ -> 1.0m
       | OpDPdy _ -> 1.0m
       | OpFwidth _ -> 1.0m
       | OpDPdxFine _ -> 1.0m
       | OpDPdyFine _ -> 1.0m
       | OpFwidthFine _ -> 1.0m
       | OpDPdxCoarse _ -> 1.0m
       | OpDPdyCoarse _ -> 1.0m
       | OpFwidthCoarse _ -> 1.0m
       | OpEmitVertex -> 1.0m
       | OpEndPrimitive -> 1.0m
       | OpEmitStreamVertex _ -> 1.0m
       | OpEndStreamPrimitive _ -> 1.0m
       | OpControlBarrier _ -> 1.0m
       | OpMemoryBarrier _ -> 1.0m
       | OpAtomicLoad _ -> 1.0m
       | OpAtomicStore _ -> 1.0m
       | OpAtomicExchange _ -> 1.0m
       | OpAtomicCompareExchange _ -> 1.0m
       | OpAtomicCompareExchangeWeak _ -> 1.0m
       | OpAtomicIIncrement _ -> 1.0m
       | OpAtomicIDecrement _ -> 1.0m
       | OpAtomicIAdd _ -> 1.0m
       | OpAtomicISub _ -> 1.0m
       | OpAtomicSMin _ -> 1.0m
       | OpAtomicUMin _ -> 1.0m
       | OpAtomicSMax _ -> 1.0m
       | OpAtomicUMax _ -> 1.0m
       | OpAtomicAnd _ -> 1.0m
       | OpAtomicOr _ -> 1.0m
       | OpAtomicXor _ -> 1.0m
       | OpPhi _ -> 1.0m
       | OpLoopMerge _ -> 1.0m
       | OpSelectionMerge _ -> 1.0m
       | OpLabel _ -> 1.0m
       | OpBranch _ -> 1.0m
       | OpBranchConditional _ -> 1.0m
       | OpSwitch _ -> 1.0m
       | OpKill -> 1.0m
       | OpReturn -> 1.0m
       | OpReturnValue _ -> 1.0m
       | OpUnreachable -> 1.0m
       | OpLifetimeStart _ -> 1.0m
       | OpLifetimeStop _ -> 1.0m
       | OpGroupAsyncCopy _ -> 1.0m
       | OpGroupWaitEvents _ -> 1.0m
       | OpGroupAll _ -> 1.0m
       | OpGroupAny _ -> 1.0m
       | OpGroupBroadcast _ -> 1.0m
       | OpGroupIAdd _ -> 1.0m
       | OpGroupFAdd _ -> 1.0m
       | OpGroupFMin _ -> 1.0m
       | OpGroupUMin _ -> 1.0m
       | OpGroupSMin _ -> 1.0m
       | OpGroupFMax _ -> 1.0m
       | OpGroupUMax _ -> 1.0m
       | OpGroupSMax _ -> 1.0m
       | OpReadPipe _ -> 1.0m
       | OpWritePipe _ -> 1.0m
       | OpReservedReadPipe _ -> 1.0m
       | OpReservedWritePipe _ -> 1.0m
       | OpReserveReadPipePackets _ -> 1.0m
       | OpReserveWritePipePackets _ -> 1.0m
       | OpCommitReadPipe _ -> 1.0m
       | OpCommitWritePipe _ -> 1.0m
       | OpIsValidReserveId _ -> 1.0m
       | OpGetNumPipePackets _ -> 1.0m
       | OpGetMaxPipePackets _ -> 1.0m
       | OpGroupReserveReadPipePackets _ -> 1.0m
       | OpGroupReserveWritePipePackets _ -> 1.0m
       | OpGroupCommitReadPipe _ -> 1.0m
       | OpGroupCommitWritePipe _ -> 1.0m
       | OpEnqueueMarker _ -> 1.0m
       | OpEnqueueKernel _ -> 1.0m
       | OpGetKernelNDrangeSubGroupCount _ -> 1.0m
       | OpGetKernelNDrangeMaxSubGroupSize _ -> 1.0m
       | OpGetKernelWorkGroupSize _ -> 1.0m
       | OpGetKernelPreferredWorkGroupSizeMultiple _ -> 1.0m
       | OpRetainEvent _ -> 1.0m
       | OpReleaseEvent _ -> 1.0m
       | OpCreateUserEvent _ -> 1.0m
       | OpIsValidEvent _ -> 1.0m
       | OpSetUserEventStatus _ -> 1.0m
       | OpCaptureEventProfilingInfo _ -> 1.0m
       | OpGetDefaultQueue _ -> 1.0m
       | OpBuildNDRange _ -> 1.0m
       | OpImageSparseSampleImplicitLod _ -> 1.0m
       | OpImageSparseSampleExplicitLod _ -> 1.0m
       | OpImageSparseSampleDrefImplicitLod _ -> 1.0m
       | OpImageSparseSampleDrefExplicitLod _ -> 1.0m
       | OpImageSparseSampleProjImplicitLod _ -> 1.0m
       | OpImageSparseSampleProjExplicitLod _ -> 1.0m
       | OpImageSparseSampleProjDrefImplicitLod _ -> 1.0m
       | OpImageSparseSampleProjDrefExplicitLod _ -> 1.0m
       | OpImageSparseFetch _ -> 1.0m
       | OpImageSparseGather _ -> 1.0m
       | OpImageSparseDrefGather _ -> 1.0m
       | OpImageSparseTexelsResident _ -> 1.0m
       | OpNoLine -> 1.0m
       | OpAtomicFlagTestAndSet _ -> 1.0m
       | OpAtomicFlagClear _ -> 1.0m
       | OpImageSparseRead _ -> 1.0m
       | OpSizeOf _ -> 1.1m
       | OpTypePipeStorage _ -> 1.1m
       | OpConstantPipeStorage _ -> 1.1m
       | OpCreatePipeFromPipeStorage _ -> 1.1m
       | OpGetKernelLocalSizeForSubgroupCount _ -> 1.1m
       | OpGetKernelMaxNumSubgroups _ -> 1.1m
       | OpTypeNamedBarrier _ -> 1.1m
       | OpNamedBarrierInitialize _ -> 1.1m
       | OpMemoryNamedBarrier _ -> 1.1m
       | OpModuleProcessed _ -> 1.1m
       | OpExecutionModeId _ -> 1.2m
       | OpDecorateId _ -> 1.2m
       | OpGroupNonUniformElect _ -> 1.3m
       | OpGroupNonUniformAll _ -> 1.3m
       | OpGroupNonUniformAny _ -> 1.3m
       | OpGroupNonUniformAllEqual _ -> 1.3m
       | OpGroupNonUniformBroadcast _ -> 1.3m
       | OpGroupNonUniformBroadcastFirst _ -> 1.3m
       | OpGroupNonUniformBallot _ -> 1.3m
       | OpGroupNonUniformInverseBallot _ -> 1.3m
       | OpGroupNonUniformBallotBitExtract _ -> 1.3m
       | OpGroupNonUniformBallotBitCount _ -> 1.3m
       | OpGroupNonUniformBallotFindLSB _ -> 1.3m
       | OpGroupNonUniformBallotFindMSB _ -> 1.3m
       | OpGroupNonUniformShuffle _ -> 1.3m
       | OpGroupNonUniformShuffleXor _ -> 1.3m
       | OpGroupNonUniformShuffleUp _ -> 1.3m
       | OpGroupNonUniformShuffleDown _ -> 1.3m
       | OpGroupNonUniformIAdd _ -> 1.3m
       | OpGroupNonUniformFAdd _ -> 1.3m
       | OpGroupNonUniformIMul _ -> 1.3m
       | OpGroupNonUniformFMul _ -> 1.3m
       | OpGroupNonUniformSMin _ -> 1.3m
       | OpGroupNonUniformUMin _ -> 1.3m
       | OpGroupNonUniformFMin _ -> 1.3m
       | OpGroupNonUniformSMax _ -> 1.3m
       | OpGroupNonUniformUMax _ -> 1.3m
       | OpGroupNonUniformFMax _ -> 1.3m
       | OpGroupNonUniformBitwiseAnd _ -> 1.3m
       | OpGroupNonUniformBitwiseOr _ -> 1.3m
       | OpGroupNonUniformBitwiseXor _ -> 1.3m
       | OpGroupNonUniformLogicalAnd _ -> 1.3m
       | OpGroupNonUniformLogicalOr _ -> 1.3m
       | OpGroupNonUniformLogicalXor _ -> 1.3m
       | OpGroupNonUniformQuadBroadcast _ -> 1.3m
       | OpGroupNonUniformQuadSwap _ -> 1.3m
       | OpCopyLogical _ -> 1.4m
       | OpPtrEqual _ -> 1.4m
       | OpPtrNotEqual _ -> 1.4m
       | OpPtrDiff _ -> 1.4m
       | OpSubgroupBallotKHR _ -> 1.0m
       | OpSubgroupFirstInvocationKHR _ -> 1.0m
       | OpSubgroupAllKHR _ -> 1.0m
       | OpSubgroupAnyKHR _ -> 1.0m
       | OpSubgroupAllEqualKHR _ -> 1.0m
       | OpSubgroupReadInvocationKHR _ -> 1.0m
       | OpGroupIAddNonUniformAMD _ -> 1.0m
       | OpGroupFAddNonUniformAMD _ -> 1.0m
       | OpGroupFMinNonUniformAMD _ -> 1.0m
       | OpGroupUMinNonUniformAMD _ -> 1.0m
       | OpGroupSMinNonUniformAMD _ -> 1.0m
       | OpGroupFMaxNonUniformAMD _ -> 1.0m
       | OpGroupUMaxNonUniformAMD _ -> 1.0m
       | OpGroupSMaxNonUniformAMD _ -> 1.0m
       | OpFragmentMaskFetchAMD _ -> 1.0m
       | OpFragmentFetchAMD _ -> 1.0m
       | OpReadClockKHR _ -> 1.0m
       | OpImageSampleFootprintNV _ -> 1.0m
       | OpGroupNonUniformPartitionNV _ -> 1.0m
       | OpWritePackedPrimitiveIndices4x8NV _ -> 1.0m
       | OpReportIntersectionNV _ -> 1.0m
       | OpIgnoreIntersectionNV -> 1.0m
       | OpTerminateRayNV -> 1.0m
       | OpTraceNV _ -> 1.0m
       | OpTypeAccelerationStructureNV _ -> 1.0m
       | OpExecuteCallableNV _ -> 1.0m
       | OpTypeCooperativeMatrixNV _ -> 1.0m
       | OpCooperativeMatrixLoadNV _ -> 1.0m
       | OpCooperativeMatrixStoreNV _ -> 1.0m
       | OpCooperativeMatrixMulAddNV _ -> 1.0m
       | OpCooperativeMatrixLengthNV _ -> 1.0m
       | OpBeginInvocationInterlockEXT -> 1.0m
       | OpEndInvocationInterlockEXT -> 1.0m
       | OpDemoteToHelperInvocationEXT -> 1.0m
       | OpIsHelperInvocationEXT _ -> 1.0m
       | OpSubgroupShuffleINTEL _ -> 1.0m
       | OpSubgroupShuffleDownINTEL _ -> 1.0m
       | OpSubgroupShuffleUpINTEL _ -> 1.0m
       | OpSubgroupShuffleXorINTEL _ -> 1.0m
       | OpSubgroupBlockReadINTEL _ -> 1.0m
       | OpSubgroupBlockWriteINTEL _ -> 1.0m
       | OpSubgroupImageBlockReadINTEL _ -> 1.0m
       | OpSubgroupImageBlockWriteINTEL _ -> 1.0m
       | OpSubgroupImageMediaBlockReadINTEL _ -> 1.0m
       | OpSubgroupImageMediaBlockWriteINTEL _ -> 1.0m
       | OpUCountLeadingZerosINTEL _ -> 1.0m
       | OpUCountTrailingZerosINTEL _ -> 1.0m
       | OpAbsISubINTEL _ -> 1.0m
       | OpAbsUSubINTEL _ -> 1.0m
       | OpIAddSatINTEL _ -> 1.0m
       | OpUAddSatINTEL _ -> 1.0m
       | OpIAverageINTEL _ -> 1.0m
       | OpUAverageINTEL _ -> 1.0m
       | OpIAverageRoundedINTEL _ -> 1.0m
       | OpUAverageRoundedINTEL _ -> 1.0m
       | OpISubSatINTEL _ -> 1.0m
       | OpUSubSatINTEL _ -> 1.0m
       | OpIMul32x16INTEL _ -> 1.0m
       | OpUMul32x16INTEL _ -> 1.0m
       | OpDecorateString _ -> 1.4m
       | OpMemberDecorateString _ -> 1.4m
       | OpVmeImageINTEL _ -> 1.0m
       | OpTypeVmeImageINTEL _ -> 1.0m
       | OpTypeAvcImePayloadINTEL _ -> 1.0m
       | OpTypeAvcRefPayloadINTEL _ -> 1.0m
       | OpTypeAvcSicPayloadINTEL _ -> 1.0m
       | OpTypeAvcMcePayloadINTEL _ -> 1.0m
       | OpTypeAvcMceResultINTEL _ -> 1.0m
       | OpTypeAvcImeResultINTEL _ -> 1.0m
       | OpTypeAvcImeResultSingleReferenceStreamoutINTEL _ -> 1.0m
       | OpTypeAvcImeResultDualReferenceStreamoutINTEL _ -> 1.0m
       | OpTypeAvcImeSingleReferenceStreaminINTEL _ -> 1.0m
       | OpTypeAvcImeDualReferenceStreaminINTEL _ -> 1.0m
       | OpTypeAvcRefResultINTEL _ -> 1.0m
       | OpTypeAvcSicResultINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultInterBaseMultiReferencePenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcMceSetInterBaseMultiReferencePenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultInterShapePenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcMceSetInterShapePenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultInterDirectionPenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcMceSetInterDirectionPenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultIntraLumaShapePenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultInterMotionVectorCostTableINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultHighPenaltyCostTableINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultMediumPenaltyCostTableINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultLowPenaltyCostTableINTEL _ -> 1.0m
       | OpSubgroupAvcMceSetMotionVectorCostFunctionINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultIntraLumaModePenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultNonDcLumaIntraPenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetDefaultIntraChromaModeBasePenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcMceSetAcOnlyHaarINTEL _ -> 1.0m
       | OpSubgroupAvcMceSetSourceInterlacedFieldPolarityINTEL _ -> 1.0m
       | OpSubgroupAvcMceSetSingleReferenceInterlacedFieldPolarityINTEL _ -> 1.0m
       | OpSubgroupAvcMceSetDualReferenceInterlacedFieldPolaritiesINTEL _ -> 1.0m
       | OpSubgroupAvcMceConvertToImePayloadINTEL _ -> 1.0m
       | OpSubgroupAvcMceConvertToImeResultINTEL _ -> 1.0m
       | OpSubgroupAvcMceConvertToRefPayloadINTEL _ -> 1.0m
       | OpSubgroupAvcMceConvertToRefResultINTEL _ -> 1.0m
       | OpSubgroupAvcMceConvertToSicPayloadINTEL _ -> 1.0m
       | OpSubgroupAvcMceConvertToSicResultINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetMotionVectorsINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetInterDistortionsINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetBestInterDistortionsINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetInterMajorShapeINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetInterMinorShapeINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetInterDirectionsINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetInterMotionVectorCountINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetInterReferenceIdsINTEL _ -> 1.0m
       | OpSubgroupAvcMceGetInterReferenceInterlacedFieldPolaritiesINTEL _ -> 1.0m
       | OpSubgroupAvcImeInitializeINTEL _ -> 1.0m
       | OpSubgroupAvcImeSetSingleReferenceINTEL _ -> 1.0m
       | OpSubgroupAvcImeSetDualReferenceINTEL _ -> 1.0m
       | OpSubgroupAvcImeRefWindowSizeINTEL _ -> 1.0m
       | OpSubgroupAvcImeAdjustRefOffsetINTEL _ -> 1.0m
       | OpSubgroupAvcImeConvertToMcePayloadINTEL _ -> 1.0m
       | OpSubgroupAvcImeSetMaxMotionVectorCountINTEL _ -> 1.0m
       | OpSubgroupAvcImeSetUnidirectionalMixDisableINTEL _ -> 1.0m
       | OpSubgroupAvcImeSetEarlySearchTerminationThresholdINTEL _ -> 1.0m
       | OpSubgroupAvcImeSetWeightedSadINTEL _ -> 1.0m
       | OpSubgroupAvcImeEvaluateWithSingleReferenceINTEL _ -> 1.0m
       | OpSubgroupAvcImeEvaluateWithDualReferenceINTEL _ -> 1.0m
       | OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminINTEL _ -> 1.0m
       | OpSubgroupAvcImeEvaluateWithDualReferenceStreaminINTEL _ -> 1.0m
       | OpSubgroupAvcImeEvaluateWithSingleReferenceStreamoutINTEL _ -> 1.0m
       | OpSubgroupAvcImeEvaluateWithDualReferenceStreamoutINTEL _ -> 1.0m
       | OpSubgroupAvcImeEvaluateWithSingleReferenceStreaminoutINTEL _ -> 1.0m
       | OpSubgroupAvcImeEvaluateWithDualReferenceStreaminoutINTEL _ -> 1.0m
       | OpSubgroupAvcImeConvertToMceResultINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetSingleReferenceStreaminINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetDualReferenceStreaminINTEL _ -> 1.0m
       | OpSubgroupAvcImeStripSingleReferenceStreamoutINTEL _ -> 1.0m
       | OpSubgroupAvcImeStripDualReferenceStreamoutINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeMotionVectorsINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeDistortionsINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetStreamoutSingleReferenceMajorShapeReferenceIdsINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeMotionVectorsINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeDistortionsINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetStreamoutDualReferenceMajorShapeReferenceIdsINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetBorderReachedINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetTruncatedSearchIndicationINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetUnidirectionalEarlySearchTerminationINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetWeightingPatternMinimumMotionVectorINTEL _ -> 1.0m
       | OpSubgroupAvcImeGetWeightingPatternMinimumDistortionINTEL _ -> 1.0m
       | OpSubgroupAvcFmeInitializeINTEL _ -> 1.0m
       | OpSubgroupAvcBmeInitializeINTEL _ -> 1.0m
       | OpSubgroupAvcRefConvertToMcePayloadINTEL _ -> 1.0m
       | OpSubgroupAvcRefSetBidirectionalMixDisableINTEL _ -> 1.0m
       | OpSubgroupAvcRefSetBilinearFilterEnableINTEL _ -> 1.0m
       | OpSubgroupAvcRefEvaluateWithSingleReferenceINTEL _ -> 1.0m
       | OpSubgroupAvcRefEvaluateWithDualReferenceINTEL _ -> 1.0m
       | OpSubgroupAvcRefEvaluateWithMultiReferenceINTEL _ -> 1.0m
       | OpSubgroupAvcRefEvaluateWithMultiReferenceInterlacedINTEL _ -> 1.0m
       | OpSubgroupAvcRefConvertToMceResultINTEL _ -> 1.0m
       | OpSubgroupAvcSicInitializeINTEL _ -> 1.0m
       | OpSubgroupAvcSicConfigureSkcINTEL _ -> 1.0m
       | OpSubgroupAvcSicConfigureIpeLumaINTEL _ -> 1.0m
       | OpSubgroupAvcSicConfigureIpeLumaChromaINTEL _ -> 1.0m
       | OpSubgroupAvcSicGetMotionVectorMaskINTEL _ -> 1.0m
       | OpSubgroupAvcSicConvertToMcePayloadINTEL _ -> 1.0m
       | OpSubgroupAvcSicSetIntraLumaShapePenaltyINTEL _ -> 1.0m
       | OpSubgroupAvcSicSetIntraLumaModeCostFunctionINTEL _ -> 1.0m
       | OpSubgroupAvcSicSetIntraChromaModeCostFunctionINTEL _ -> 1.0m
       | OpSubgroupAvcSicSetBilinearFilterEnableINTEL _ -> 1.0m
       | OpSubgroupAvcSicSetSkcForwardTransformEnableINTEL _ -> 1.0m
       | OpSubgroupAvcSicSetBlockBasedRawSkipSadINTEL _ -> 1.0m
       | OpSubgroupAvcSicEvaluateIpeINTEL _ -> 1.0m
       | OpSubgroupAvcSicEvaluateWithSingleReferenceINTEL _ -> 1.0m
       | OpSubgroupAvcSicEvaluateWithDualReferenceINTEL _ -> 1.0m
       | OpSubgroupAvcSicEvaluateWithMultiReferenceINTEL _ -> 1.0m
       | OpSubgroupAvcSicEvaluateWithMultiReferenceInterlacedINTEL _ -> 1.0m
       | OpSubgroupAvcSicConvertToMceResultINTEL _ -> 1.0m
       | OpSubgroupAvcSicGetIpeLumaShapeINTEL _ -> 1.0m
       | OpSubgroupAvcSicGetBestIpeLumaDistortionINTEL _ -> 1.0m
       | OpSubgroupAvcSicGetBestIpeChromaDistortionINTEL _ -> 1.0m
       | OpSubgroupAvcSicGetPackedIpeLumaModesINTEL _ -> 1.0m
       | OpSubgroupAvcSicGetIpeChromaModeINTEL _ -> 1.0m
       | OpSubgroupAvcSicGetPackedSkcLumaCountThresholdINTEL _ -> 1.0m
       | OpSubgroupAvcSicGetPackedSkcLumaSumThresholdINTEL _ -> 1.0m
       | OpSubgroupAvcSicGetInterRawSadsINTEL _ -> 1.0m

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
            | ExecutionMode.LocalSizeHintId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.PostDepthCoverage -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DenormPreserve(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.DenormFlushToZero(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SignedZeroInfNanPreserve(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTE(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTZ(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.StencilRefReplacingEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputLinesNV -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputPrimitivesNV(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.DerivativeGroupQuadsNV -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DerivativeGroupLinearNV -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputTrianglesNV -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PixelInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PixelInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SampleInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SampleInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.ShadingRateInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.ShadingRateInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
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
        | OpTypeFloat(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
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
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value))
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
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value))
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
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value))
            stream.WriteOption(arg3, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value))
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
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value))
            stream.WriteOption(arg4, fun v -> 
            match v with
            | MemoryAccess.None -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Volatile -> stream.WriteUInt32(v.Value)
            | MemoryAccess.Aligned(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.Nontemporal -> stream.WriteUInt32(v.Value)
            | MemoryAccess.MakePointerAvailable(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.MakePointerVisible(v_arg0) -> stream.WriteUInt32(v.Value);stream.WriteUInt32(v_arg0)
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value))
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
            | Decoration.ExplicitInterpAMD -> stream.WriteUInt32(arg1.Value)
            | Decoration.OverrideCoverageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PassthroughNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.ViewportRelativeNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.SecondaryViewportRelativeNV(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PerPrimitiveNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerViewNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerTaskNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerVertexNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonUniform -> stream.WriteUInt32(arg1.Value)
            | Decoration.RestrictPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.AliasedPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.CounterBuffer(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.UserSemantic(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.UserTypeGOOGLE(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
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
            | Decoration.ExplicitInterpAMD -> stream.WriteUInt32(arg2.Value)
            | Decoration.OverrideCoverageNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PassthroughNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.ViewportRelativeNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.SecondaryViewportRelativeNV(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PerPrimitiveNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerViewNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerTaskNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerVertexNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.NonUniform -> stream.WriteUInt32(arg2.Value)
            | Decoration.RestrictPointer -> stream.WriteUInt32(arg2.Value)
            | Decoration.AliasedPointer -> stream.WriteUInt32(arg2.Value)
            | Decoration.CounterBuffer(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.UserSemantic(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
            | Decoration.UserTypeGOOGLE(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
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
            | ExecutionMode.LocalSizeHintId(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.PostDepthCoverage -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DenormPreserve(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.DenormFlushToZero(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.SignedZeroInfNanPreserve(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTE(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.RoundingModeRTZ(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.StencilRefReplacingEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputLinesNV -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputPrimitivesNV(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | ExecutionMode.DerivativeGroupQuadsNV -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.DerivativeGroupLinearNV -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.OutputTrianglesNV -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PixelInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.PixelInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SampleInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.SampleInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.ShadingRateInterlockOrderedEXT -> stream.WriteUInt32(arg1.Value)
            | ExecutionMode.ShadingRateInterlockUnorderedEXT -> stream.WriteUInt32(arg1.Value)
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
            | Decoration.ExplicitInterpAMD -> stream.WriteUInt32(arg1.Value)
            | Decoration.OverrideCoverageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PassthroughNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.ViewportRelativeNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.SecondaryViewportRelativeNV(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PerPrimitiveNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerViewNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerTaskNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerVertexNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonUniform -> stream.WriteUInt32(arg1.Value)
            | Decoration.RestrictPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.AliasedPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.CounterBuffer(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.UserSemantic(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.UserTypeGOOGLE(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
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
        | OpSubgroupBallotKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpSubgroupFirstInvocationKHR(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
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
        | OpSubgroupReadInvocationKHR(arg0, arg1, arg2, arg3) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
            stream.WriteUInt32(arg3)
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
            | ImageOperands.ZeroExtend -> stream.WriteUInt32(v.Value))
        | OpGroupNonUniformPartitionNV(arg0, arg1, arg2) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
            stream.WriteUInt32(arg2)
        | OpWritePackedPrimitiveIndices4x8NV(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
        | OpReportIntersectionNV(arg0, arg1, arg2, arg3) ->
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
        | OpTypeAccelerationStructureNV(arg0) ->
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
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value))
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
            | MemoryAccess.NonPrivatePointer -> stream.WriteUInt32(v.Value))
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
        | OpDemoteToHelperInvocationEXT ->
            ()
        | OpIsHelperInvocationEXT(arg0, arg1) ->
            stream.WriteUInt32(arg0)
            stream.WriteUInt32(arg1)
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
            | Decoration.ExplicitInterpAMD -> stream.WriteUInt32(arg1.Value)
            | Decoration.OverrideCoverageNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PassthroughNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.ViewportRelativeNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.SecondaryViewportRelativeNV(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.PerPrimitiveNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerViewNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerTaskNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.PerVertexNV -> stream.WriteUInt32(arg1.Value)
            | Decoration.NonUniform -> stream.WriteUInt32(arg1.Value)
            | Decoration.RestrictPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.AliasedPointer -> stream.WriteUInt32(arg1.Value)
            | Decoration.CounterBuffer(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteUInt32(arg1_arg0)
            | Decoration.UserSemantic(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
            | Decoration.UserTypeGOOGLE(arg1_arg0) -> stream.WriteUInt32(arg1.Value);stream.WriteString(arg1_arg0)
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
            | Decoration.ExplicitInterpAMD -> stream.WriteUInt32(arg2.Value)
            | Decoration.OverrideCoverageNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PassthroughNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.ViewportRelativeNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.SecondaryViewportRelativeNV(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.PerPrimitiveNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerViewNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerTaskNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.PerVertexNV -> stream.WriteUInt32(arg2.Value)
            | Decoration.NonUniform -> stream.WriteUInt32(arg2.Value)
            | Decoration.RestrictPointer -> stream.WriteUInt32(arg2.Value)
            | Decoration.AliasedPointer -> stream.WriteUInt32(arg2.Value)
            | Decoration.CounterBuffer(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteUInt32(arg2_arg0)
            | Decoration.UserSemantic(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
            | Decoration.UserTypeGOOGLE(arg2_arg0) -> stream.WriteUInt32(arg2.Value);stream.WriteString(arg2_arg0)
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
            OpExecutionMode(stream.ReadUInt32(), match stream.ReadUInt32() with | 0u -> ExecutionMode.Invocations(stream.ReadUInt32()) | 1u -> ExecutionMode.SpacingEqual | 2u -> ExecutionMode.SpacingFractionalEven | 3u -> ExecutionMode.SpacingFractionalOdd | 4u -> ExecutionMode.VertexOrderCw | 5u -> ExecutionMode.VertexOrderCcw | 6u -> ExecutionMode.PixelCenterInteger | 7u -> ExecutionMode.OriginUpperLeft | 8u -> ExecutionMode.OriginLowerLeft | 9u -> ExecutionMode.EarlyFragmentTests | 10u -> ExecutionMode.PointMode | 11u -> ExecutionMode.Xfb | 12u -> ExecutionMode.DepthReplacing | 14u -> ExecutionMode.DepthGreater | 15u -> ExecutionMode.DepthLess | 16u -> ExecutionMode.DepthUnchanged | 17u -> ExecutionMode.LocalSize(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 18u -> ExecutionMode.LocalSizeHint(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 19u -> ExecutionMode.InputPoints | 20u -> ExecutionMode.InputLines | 21u -> ExecutionMode.InputLinesAdjacency | 22u -> ExecutionMode.Triangles | 23u -> ExecutionMode.InputTrianglesAdjacency | 24u -> ExecutionMode.Quads | 25u -> ExecutionMode.Isolines | 26u -> ExecutionMode.OutputVertices(stream.ReadUInt32()) | 27u -> ExecutionMode.OutputPoints | 28u -> ExecutionMode.OutputLineStrip | 29u -> ExecutionMode.OutputTriangleStrip | 30u -> ExecutionMode.VecTypeHint(stream.ReadUInt32()) | 31u -> ExecutionMode.ContractionOff | 33u -> ExecutionMode.Initializer | 34u -> ExecutionMode.Finalizer | 35u -> ExecutionMode.SubgroupSize(stream.ReadUInt32()) | 36u -> ExecutionMode.SubgroupsPerWorkgroup(stream.ReadUInt32()) | 37u -> ExecutionMode.SubgroupsPerWorkgroupId(stream.ReadUInt32()) | 38u -> ExecutionMode.LocalSizeId(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 39u -> ExecutionMode.LocalSizeHintId(stream.ReadUInt32()) | 4446u -> ExecutionMode.PostDepthCoverage | 4459u -> ExecutionMode.DenormPreserve(stream.ReadUInt32()) | 4460u -> ExecutionMode.DenormFlushToZero(stream.ReadUInt32()) | 4461u -> ExecutionMode.SignedZeroInfNanPreserve(stream.ReadUInt32()) | 4462u -> ExecutionMode.RoundingModeRTE(stream.ReadUInt32()) | 4463u -> ExecutionMode.RoundingModeRTZ(stream.ReadUInt32()) | 5027u -> ExecutionMode.StencilRefReplacingEXT | 5269u -> ExecutionMode.OutputLinesNV | 5270u -> ExecutionMode.OutputPrimitivesNV(stream.ReadUInt32()) | 5289u -> ExecutionMode.DerivativeGroupQuadsNV | 5290u -> ExecutionMode.DerivativeGroupLinearNV | 5298u -> ExecutionMode.OutputTrianglesNV | 5366u -> ExecutionMode.PixelInterlockOrderedEXT | 5367u -> ExecutionMode.PixelInterlockUnorderedEXT | 5368u -> ExecutionMode.SampleInterlockOrderedEXT | 5369u -> ExecutionMode.SampleInterlockUnorderedEXT | 5370u -> ExecutionMode.ShadingRateInterlockOrderedEXT | 5371u -> ExecutionMode.ShadingRateInterlockUnorderedEXT | _ -> failwith "invalid" )
        | 17us ->
            OpCapability(stream.ReadEnum())
        | 19us ->
            OpTypeVoid(stream.ReadUInt32())
        | 20us ->
            OpTypeBool(stream.ReadUInt32())
        | 21us ->
            OpTypeInt(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 22us ->
            OpTypeFloat(stream.ReadUInt32(), stream.ReadUInt32())
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
            OpLoad(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | _ -> failwith "invalid" ))
        | 62us ->
            OpStore(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | _ -> failwith "invalid" ))
        | 63us ->
            OpCopyMemory(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | _ -> failwith "invalid" ), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | _ -> failwith "invalid" ))
        | 64us ->
            OpCopyMemorySized(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | _ -> failwith "invalid" ), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | _ -> failwith "invalid" ))
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
            OpDecorate(stream.ReadUInt32(), match stream.ReadUInt32() with | 0u -> Decoration.RelaxedPrecision | 1u -> Decoration.SpecId(stream.ReadUInt32()) | 2u -> Decoration.Block | 3u -> Decoration.BufferBlock | 4u -> Decoration.RowMajor | 5u -> Decoration.ColMajor | 6u -> Decoration.ArrayStride(stream.ReadUInt32()) | 7u -> Decoration.MatrixStride(stream.ReadUInt32()) | 8u -> Decoration.GLSLShared | 9u -> Decoration.GLSLPacked | 10u -> Decoration.CPacked | 11u -> Decoration.BuiltIn(stream.ReadEnum()) | 13u -> Decoration.NoPerspective | 14u -> Decoration.Flat | 15u -> Decoration.Patch | 16u -> Decoration.Centroid | 17u -> Decoration.Sample | 18u -> Decoration.Invariant | 19u -> Decoration.Restrict | 20u -> Decoration.Aliased | 21u -> Decoration.Volatile | 22u -> Decoration.Constant | 23u -> Decoration.Coherent | 24u -> Decoration.NonWritable | 25u -> Decoration.NonReadable | 26u -> Decoration.Uniform | 27u -> Decoration.UniformId(stream.ReadUInt32()) | 28u -> Decoration.SaturatedConversion | 29u -> Decoration.Stream(stream.ReadUInt32()) | 30u -> Decoration.Location(stream.ReadUInt32()) | 31u -> Decoration.Component(stream.ReadUInt32()) | 32u -> Decoration.Index(stream.ReadUInt32()) | 33u -> Decoration.Binding(stream.ReadUInt32()) | 34u -> Decoration.DescriptorSet(stream.ReadUInt32()) | 35u -> Decoration.Offset(stream.ReadUInt32()) | 36u -> Decoration.XfbBuffer(stream.ReadUInt32()) | 37u -> Decoration.XfbStride(stream.ReadUInt32()) | 38u -> Decoration.FuncParamAttr(stream.ReadEnum()) | 39u -> Decoration.FPRoundingMode(stream.ReadEnum()) | 40u -> Decoration.FPFastMathMode(stream.ReadEnum()) | 41u -> Decoration.LinkageAttributes(stream.ReadString(), stream.ReadEnum()) | 42u -> Decoration.NoContraction | 43u -> Decoration.InputAttachmentIndex(stream.ReadUInt32()) | 44u -> Decoration.Alignment(stream.ReadUInt32()) | 45u -> Decoration.MaxByteOffset(stream.ReadUInt32()) | 46u -> Decoration.AlignmentId(stream.ReadUInt32()) | 47u -> Decoration.MaxByteOffsetId(stream.ReadUInt32()) | 4469u -> Decoration.NoSignedWrap | 4470u -> Decoration.NoUnsignedWrap | 4999u -> Decoration.ExplicitInterpAMD | 5248u -> Decoration.OverrideCoverageNV | 5250u -> Decoration.PassthroughNV | 5252u -> Decoration.ViewportRelativeNV | 5256u -> Decoration.SecondaryViewportRelativeNV(stream.ReadUInt32()) | 5271u -> Decoration.PerPrimitiveNV | 5272u -> Decoration.PerViewNV | 5273u -> Decoration.PerTaskNV | 5285u -> Decoration.PerVertexNV | 5300u -> Decoration.NonUniform | 5355u -> Decoration.RestrictPointer | 5356u -> Decoration.AliasedPointer | 5634u -> Decoration.CounterBuffer(stream.ReadUInt32()) | 5635u -> Decoration.UserSemantic(stream.ReadString()) | 5636u -> Decoration.UserTypeGOOGLE(stream.ReadString()) | _ -> failwith "invalid" )
        | 72us ->
            OpMemberDecorate(stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0u -> Decoration.RelaxedPrecision | 1u -> Decoration.SpecId(stream.ReadUInt32()) | 2u -> Decoration.Block | 3u -> Decoration.BufferBlock | 4u -> Decoration.RowMajor | 5u -> Decoration.ColMajor | 6u -> Decoration.ArrayStride(stream.ReadUInt32()) | 7u -> Decoration.MatrixStride(stream.ReadUInt32()) | 8u -> Decoration.GLSLShared | 9u -> Decoration.GLSLPacked | 10u -> Decoration.CPacked | 11u -> Decoration.BuiltIn(stream.ReadEnum()) | 13u -> Decoration.NoPerspective | 14u -> Decoration.Flat | 15u -> Decoration.Patch | 16u -> Decoration.Centroid | 17u -> Decoration.Sample | 18u -> Decoration.Invariant | 19u -> Decoration.Restrict | 20u -> Decoration.Aliased | 21u -> Decoration.Volatile | 22u -> Decoration.Constant | 23u -> Decoration.Coherent | 24u -> Decoration.NonWritable | 25u -> Decoration.NonReadable | 26u -> Decoration.Uniform | 27u -> Decoration.UniformId(stream.ReadUInt32()) | 28u -> Decoration.SaturatedConversion | 29u -> Decoration.Stream(stream.ReadUInt32()) | 30u -> Decoration.Location(stream.ReadUInt32()) | 31u -> Decoration.Component(stream.ReadUInt32()) | 32u -> Decoration.Index(stream.ReadUInt32()) | 33u -> Decoration.Binding(stream.ReadUInt32()) | 34u -> Decoration.DescriptorSet(stream.ReadUInt32()) | 35u -> Decoration.Offset(stream.ReadUInt32()) | 36u -> Decoration.XfbBuffer(stream.ReadUInt32()) | 37u -> Decoration.XfbStride(stream.ReadUInt32()) | 38u -> Decoration.FuncParamAttr(stream.ReadEnum()) | 39u -> Decoration.FPRoundingMode(stream.ReadEnum()) | 40u -> Decoration.FPFastMathMode(stream.ReadEnum()) | 41u -> Decoration.LinkageAttributes(stream.ReadString(), stream.ReadEnum()) | 42u -> Decoration.NoContraction | 43u -> Decoration.InputAttachmentIndex(stream.ReadUInt32()) | 44u -> Decoration.Alignment(stream.ReadUInt32()) | 45u -> Decoration.MaxByteOffset(stream.ReadUInt32()) | 46u -> Decoration.AlignmentId(stream.ReadUInt32()) | 47u -> Decoration.MaxByteOffsetId(stream.ReadUInt32()) | 4469u -> Decoration.NoSignedWrap | 4470u -> Decoration.NoUnsignedWrap | 4999u -> Decoration.ExplicitInterpAMD | 5248u -> Decoration.OverrideCoverageNV | 5250u -> Decoration.PassthroughNV | 5252u -> Decoration.ViewportRelativeNV | 5256u -> Decoration.SecondaryViewportRelativeNV(stream.ReadUInt32()) | 5271u -> Decoration.PerPrimitiveNV | 5272u -> Decoration.PerViewNV | 5273u -> Decoration.PerTaskNV | 5285u -> Decoration.PerVertexNV | 5300u -> Decoration.NonUniform | 5355u -> Decoration.RestrictPointer | 5356u -> Decoration.AliasedPointer | 5634u -> Decoration.CounterBuffer(stream.ReadUInt32()) | 5635u -> Decoration.UserSemantic(stream.ReadString()) | 5636u -> Decoration.UserTypeGOOGLE(stream.ReadString()) | _ -> failwith "invalid" )
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
            OpImageSampleImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 88us ->
            OpImageSampleExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" )
        | 89us ->
            OpImageSampleDrefImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 90us ->
            OpImageSampleDrefExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" )
        | 91us ->
            OpImageSampleProjImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 92us ->
            OpImageSampleProjExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" )
        | 93us ->
            OpImageSampleProjDrefImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 94us ->
            OpImageSampleProjDrefExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" )
        | 95us ->
            OpImageFetch(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 96us ->
            OpImageGather(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 97us ->
            OpImageDrefGather(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 98us ->
            OpImageRead(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 99us ->
            OpImageWrite(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
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
            OpLoopMerge(stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0x0000u -> LoopControl.None | 0x0001u -> LoopControl.Unroll | 0x0002u -> LoopControl.DontUnroll | 0x0004u -> LoopControl.DependencyInfinite | 0x0008u -> LoopControl.DependencyLength(stream.ReadUInt32()) | 0x0010u -> LoopControl.MinIterations(stream.ReadUInt32()) | 0x0020u -> LoopControl.MaxIterations(stream.ReadUInt32()) | 0x0040u -> LoopControl.IterationMultiple(stream.ReadUInt32()) | 0x0080u -> LoopControl.PeelCount(stream.ReadUInt32()) | 0x0100u -> LoopControl.PartialCount(stream.ReadUInt32()) | _ -> failwith "invalid" )
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
            OpImageSparseSampleImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 306us ->
            OpImageSparseSampleExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" )
        | 307us ->
            OpImageSparseSampleDrefImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 308us ->
            OpImageSparseSampleDrefExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" )
        | 309us ->
            OpImageSparseSampleProjImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 310us ->
            OpImageSparseSampleProjExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" )
        | 311us ->
            OpImageSparseSampleProjDrefImplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 312us ->
            OpImageSparseSampleProjDrefExplicitLod(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" )
        | 313us ->
            OpImageSparseFetch(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 314us ->
            OpImageSparseGather(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 315us ->
            OpImageSparseDrefGather(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 316us ->
            OpImageSparseTexelsResident(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 317us ->
            OpNoLine
        | 318us ->
            OpAtomicFlagTestAndSet(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 319us ->
            OpAtomicFlagClear(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 320us ->
            OpImageSparseRead(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
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
            OpExecutionModeId(stream.ReadUInt32(), match stream.ReadUInt32() with | 0u -> ExecutionMode.Invocations(stream.ReadUInt32()) | 1u -> ExecutionMode.SpacingEqual | 2u -> ExecutionMode.SpacingFractionalEven | 3u -> ExecutionMode.SpacingFractionalOdd | 4u -> ExecutionMode.VertexOrderCw | 5u -> ExecutionMode.VertexOrderCcw | 6u -> ExecutionMode.PixelCenterInteger | 7u -> ExecutionMode.OriginUpperLeft | 8u -> ExecutionMode.OriginLowerLeft | 9u -> ExecutionMode.EarlyFragmentTests | 10u -> ExecutionMode.PointMode | 11u -> ExecutionMode.Xfb | 12u -> ExecutionMode.DepthReplacing | 14u -> ExecutionMode.DepthGreater | 15u -> ExecutionMode.DepthLess | 16u -> ExecutionMode.DepthUnchanged | 17u -> ExecutionMode.LocalSize(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 18u -> ExecutionMode.LocalSizeHint(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 19u -> ExecutionMode.InputPoints | 20u -> ExecutionMode.InputLines | 21u -> ExecutionMode.InputLinesAdjacency | 22u -> ExecutionMode.Triangles | 23u -> ExecutionMode.InputTrianglesAdjacency | 24u -> ExecutionMode.Quads | 25u -> ExecutionMode.Isolines | 26u -> ExecutionMode.OutputVertices(stream.ReadUInt32()) | 27u -> ExecutionMode.OutputPoints | 28u -> ExecutionMode.OutputLineStrip | 29u -> ExecutionMode.OutputTriangleStrip | 30u -> ExecutionMode.VecTypeHint(stream.ReadUInt32()) | 31u -> ExecutionMode.ContractionOff | 33u -> ExecutionMode.Initializer | 34u -> ExecutionMode.Finalizer | 35u -> ExecutionMode.SubgroupSize(stream.ReadUInt32()) | 36u -> ExecutionMode.SubgroupsPerWorkgroup(stream.ReadUInt32()) | 37u -> ExecutionMode.SubgroupsPerWorkgroupId(stream.ReadUInt32()) | 38u -> ExecutionMode.LocalSizeId(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32()) | 39u -> ExecutionMode.LocalSizeHintId(stream.ReadUInt32()) | 4446u -> ExecutionMode.PostDepthCoverage | 4459u -> ExecutionMode.DenormPreserve(stream.ReadUInt32()) | 4460u -> ExecutionMode.DenormFlushToZero(stream.ReadUInt32()) | 4461u -> ExecutionMode.SignedZeroInfNanPreserve(stream.ReadUInt32()) | 4462u -> ExecutionMode.RoundingModeRTE(stream.ReadUInt32()) | 4463u -> ExecutionMode.RoundingModeRTZ(stream.ReadUInt32()) | 5027u -> ExecutionMode.StencilRefReplacingEXT | 5269u -> ExecutionMode.OutputLinesNV | 5270u -> ExecutionMode.OutputPrimitivesNV(stream.ReadUInt32()) | 5289u -> ExecutionMode.DerivativeGroupQuadsNV | 5290u -> ExecutionMode.DerivativeGroupLinearNV | 5298u -> ExecutionMode.OutputTrianglesNV | 5366u -> ExecutionMode.PixelInterlockOrderedEXT | 5367u -> ExecutionMode.PixelInterlockUnorderedEXT | 5368u -> ExecutionMode.SampleInterlockOrderedEXT | 5369u -> ExecutionMode.SampleInterlockUnorderedEXT | 5370u -> ExecutionMode.ShadingRateInterlockOrderedEXT | 5371u -> ExecutionMode.ShadingRateInterlockUnorderedEXT | _ -> failwith "invalid" )
        | 332us ->
            OpDecorateId(stream.ReadUInt32(), match stream.ReadUInt32() with | 0u -> Decoration.RelaxedPrecision | 1u -> Decoration.SpecId(stream.ReadUInt32()) | 2u -> Decoration.Block | 3u -> Decoration.BufferBlock | 4u -> Decoration.RowMajor | 5u -> Decoration.ColMajor | 6u -> Decoration.ArrayStride(stream.ReadUInt32()) | 7u -> Decoration.MatrixStride(stream.ReadUInt32()) | 8u -> Decoration.GLSLShared | 9u -> Decoration.GLSLPacked | 10u -> Decoration.CPacked | 11u -> Decoration.BuiltIn(stream.ReadEnum()) | 13u -> Decoration.NoPerspective | 14u -> Decoration.Flat | 15u -> Decoration.Patch | 16u -> Decoration.Centroid | 17u -> Decoration.Sample | 18u -> Decoration.Invariant | 19u -> Decoration.Restrict | 20u -> Decoration.Aliased | 21u -> Decoration.Volatile | 22u -> Decoration.Constant | 23u -> Decoration.Coherent | 24u -> Decoration.NonWritable | 25u -> Decoration.NonReadable | 26u -> Decoration.Uniform | 27u -> Decoration.UniformId(stream.ReadUInt32()) | 28u -> Decoration.SaturatedConversion | 29u -> Decoration.Stream(stream.ReadUInt32()) | 30u -> Decoration.Location(stream.ReadUInt32()) | 31u -> Decoration.Component(stream.ReadUInt32()) | 32u -> Decoration.Index(stream.ReadUInt32()) | 33u -> Decoration.Binding(stream.ReadUInt32()) | 34u -> Decoration.DescriptorSet(stream.ReadUInt32()) | 35u -> Decoration.Offset(stream.ReadUInt32()) | 36u -> Decoration.XfbBuffer(stream.ReadUInt32()) | 37u -> Decoration.XfbStride(stream.ReadUInt32()) | 38u -> Decoration.FuncParamAttr(stream.ReadEnum()) | 39u -> Decoration.FPRoundingMode(stream.ReadEnum()) | 40u -> Decoration.FPFastMathMode(stream.ReadEnum()) | 41u -> Decoration.LinkageAttributes(stream.ReadString(), stream.ReadEnum()) | 42u -> Decoration.NoContraction | 43u -> Decoration.InputAttachmentIndex(stream.ReadUInt32()) | 44u -> Decoration.Alignment(stream.ReadUInt32()) | 45u -> Decoration.MaxByteOffset(stream.ReadUInt32()) | 46u -> Decoration.AlignmentId(stream.ReadUInt32()) | 47u -> Decoration.MaxByteOffsetId(stream.ReadUInt32()) | 4469u -> Decoration.NoSignedWrap | 4470u -> Decoration.NoUnsignedWrap | 4999u -> Decoration.ExplicitInterpAMD | 5248u -> Decoration.OverrideCoverageNV | 5250u -> Decoration.PassthroughNV | 5252u -> Decoration.ViewportRelativeNV | 5256u -> Decoration.SecondaryViewportRelativeNV(stream.ReadUInt32()) | 5271u -> Decoration.PerPrimitiveNV | 5272u -> Decoration.PerViewNV | 5273u -> Decoration.PerTaskNV | 5285u -> Decoration.PerVertexNV | 5300u -> Decoration.NonUniform | 5355u -> Decoration.RestrictPointer | 5356u -> Decoration.AliasedPointer | 5634u -> Decoration.CounterBuffer(stream.ReadUInt32()) | 5635u -> Decoration.UserSemantic(stream.ReadString()) | 5636u -> Decoration.UserTypeGOOGLE(stream.ReadString()) | _ -> failwith "invalid" )
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
        | 4421us ->
            OpSubgroupBallotKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4422us ->
            OpSubgroupFirstInvocationKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4428us ->
            OpSubgroupAllKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4429us ->
            OpSubgroupAnyKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4430us ->
            OpSubgroupAllEqualKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 4432us ->
            OpSubgroupReadInvocationKHR(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
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
        | 5283us ->
            OpImageSampleFootprintNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> ImageOperands.None | 0x0001u -> ImageOperands.Bias(stream.ReadUInt32()) | 0x0002u -> ImageOperands.Lod(stream.ReadUInt32()) | 0x0004u -> ImageOperands.Grad(stream.ReadUInt32(), stream.ReadUInt32()) | 0x0008u -> ImageOperands.ConstOffset(stream.ReadUInt32()) | 0x0010u -> ImageOperands.Offset(stream.ReadUInt32()) | 0x0020u -> ImageOperands.ConstOffsets(stream.ReadUInt32()) | 0x0040u -> ImageOperands.Sample(stream.ReadUInt32()) | 0x0080u -> ImageOperands.MinLod(stream.ReadUInt32()) | 0x0100u -> ImageOperands.MakeTexelAvailable(stream.ReadUInt32()) | 0x0200u -> ImageOperands.MakeTexelVisible(stream.ReadUInt32()) | 0x0400u -> ImageOperands.NonPrivateTexel | 0x0800u -> ImageOperands.VolatileTexel | 0x1000u -> ImageOperands.SignExtend | 0x2000u -> ImageOperands.ZeroExtend | _ -> failwith "invalid" ))
        | 5296us ->
            OpGroupNonUniformPartitionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5299us ->
            OpWritePackedPrimitiveIndices4x8NV(stream.ReadUInt32(), stream.ReadUInt32())
        | 5334us ->
            OpReportIntersectionNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5335us ->
            OpIgnoreIntersectionNV
        | 5336us ->
            OpTerminateRayNV
        | 5337us ->
            OpTraceNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5341us ->
            OpTypeAccelerationStructureNV(stream.ReadUInt32())
        | 5344us ->
            OpExecuteCallableNV(stream.ReadUInt32(), stream.ReadUInt32())
        | 5358us ->
            OpTypeCooperativeMatrixNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5359us ->
            OpCooperativeMatrixLoadNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | _ -> failwith "invalid" ))
        | 5360us ->
            OpCooperativeMatrixStoreNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadOption(fun () -> match stream.ReadUInt32() with | 0x0000u -> MemoryAccess.None | 0x0001u -> MemoryAccess.Volatile | 0x0002u -> MemoryAccess.Aligned(stream.ReadUInt32()) | 0x0004u -> MemoryAccess.Nontemporal | 0x0008u -> MemoryAccess.MakePointerAvailable(stream.ReadUInt32()) | 0x0010u -> MemoryAccess.MakePointerVisible(stream.ReadUInt32()) | 0x0020u -> MemoryAccess.NonPrivatePointer | _ -> failwith "invalid" ))
        | 5361us ->
            OpCooperativeMatrixMulAddNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5362us ->
            OpCooperativeMatrixLengthNV(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())
        | 5364us ->
            OpBeginInvocationInterlockEXT
        | 5365us ->
            OpEndInvocationInterlockEXT
        | 5380us ->
            OpDemoteToHelperInvocationEXT
        | 5381us ->
            OpIsHelperInvocationEXT(stream.ReadUInt32(), stream.ReadUInt32())
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
        | 5632us ->
            OpDecorateString(stream.ReadUInt32(), match stream.ReadUInt32() with | 0u -> Decoration.RelaxedPrecision | 1u -> Decoration.SpecId(stream.ReadUInt32()) | 2u -> Decoration.Block | 3u -> Decoration.BufferBlock | 4u -> Decoration.RowMajor | 5u -> Decoration.ColMajor | 6u -> Decoration.ArrayStride(stream.ReadUInt32()) | 7u -> Decoration.MatrixStride(stream.ReadUInt32()) | 8u -> Decoration.GLSLShared | 9u -> Decoration.GLSLPacked | 10u -> Decoration.CPacked | 11u -> Decoration.BuiltIn(stream.ReadEnum()) | 13u -> Decoration.NoPerspective | 14u -> Decoration.Flat | 15u -> Decoration.Patch | 16u -> Decoration.Centroid | 17u -> Decoration.Sample | 18u -> Decoration.Invariant | 19u -> Decoration.Restrict | 20u -> Decoration.Aliased | 21u -> Decoration.Volatile | 22u -> Decoration.Constant | 23u -> Decoration.Coherent | 24u -> Decoration.NonWritable | 25u -> Decoration.NonReadable | 26u -> Decoration.Uniform | 27u -> Decoration.UniformId(stream.ReadUInt32()) | 28u -> Decoration.SaturatedConversion | 29u -> Decoration.Stream(stream.ReadUInt32()) | 30u -> Decoration.Location(stream.ReadUInt32()) | 31u -> Decoration.Component(stream.ReadUInt32()) | 32u -> Decoration.Index(stream.ReadUInt32()) | 33u -> Decoration.Binding(stream.ReadUInt32()) | 34u -> Decoration.DescriptorSet(stream.ReadUInt32()) | 35u -> Decoration.Offset(stream.ReadUInt32()) | 36u -> Decoration.XfbBuffer(stream.ReadUInt32()) | 37u -> Decoration.XfbStride(stream.ReadUInt32()) | 38u -> Decoration.FuncParamAttr(stream.ReadEnum()) | 39u -> Decoration.FPRoundingMode(stream.ReadEnum()) | 40u -> Decoration.FPFastMathMode(stream.ReadEnum()) | 41u -> Decoration.LinkageAttributes(stream.ReadString(), stream.ReadEnum()) | 42u -> Decoration.NoContraction | 43u -> Decoration.InputAttachmentIndex(stream.ReadUInt32()) | 44u -> Decoration.Alignment(stream.ReadUInt32()) | 45u -> Decoration.MaxByteOffset(stream.ReadUInt32()) | 46u -> Decoration.AlignmentId(stream.ReadUInt32()) | 47u -> Decoration.MaxByteOffsetId(stream.ReadUInt32()) | 4469u -> Decoration.NoSignedWrap | 4470u -> Decoration.NoUnsignedWrap | 4999u -> Decoration.ExplicitInterpAMD | 5248u -> Decoration.OverrideCoverageNV | 5250u -> Decoration.PassthroughNV | 5252u -> Decoration.ViewportRelativeNV | 5256u -> Decoration.SecondaryViewportRelativeNV(stream.ReadUInt32()) | 5271u -> Decoration.PerPrimitiveNV | 5272u -> Decoration.PerViewNV | 5273u -> Decoration.PerTaskNV | 5285u -> Decoration.PerVertexNV | 5300u -> Decoration.NonUniform | 5355u -> Decoration.RestrictPointer | 5356u -> Decoration.AliasedPointer | 5634u -> Decoration.CounterBuffer(stream.ReadUInt32()) | 5635u -> Decoration.UserSemantic(stream.ReadString()) | 5636u -> Decoration.UserTypeGOOGLE(stream.ReadString()) | _ -> failwith "invalid" )
        | 5633us ->
            OpMemberDecorateString(stream.ReadUInt32(), stream.ReadUInt32(), match stream.ReadUInt32() with | 0u -> Decoration.RelaxedPrecision | 1u -> Decoration.SpecId(stream.ReadUInt32()) | 2u -> Decoration.Block | 3u -> Decoration.BufferBlock | 4u -> Decoration.RowMajor | 5u -> Decoration.ColMajor | 6u -> Decoration.ArrayStride(stream.ReadUInt32()) | 7u -> Decoration.MatrixStride(stream.ReadUInt32()) | 8u -> Decoration.GLSLShared | 9u -> Decoration.GLSLPacked | 10u -> Decoration.CPacked | 11u -> Decoration.BuiltIn(stream.ReadEnum()) | 13u -> Decoration.NoPerspective | 14u -> Decoration.Flat | 15u -> Decoration.Patch | 16u -> Decoration.Centroid | 17u -> Decoration.Sample | 18u -> Decoration.Invariant | 19u -> Decoration.Restrict | 20u -> Decoration.Aliased | 21u -> Decoration.Volatile | 22u -> Decoration.Constant | 23u -> Decoration.Coherent | 24u -> Decoration.NonWritable | 25u -> Decoration.NonReadable | 26u -> Decoration.Uniform | 27u -> Decoration.UniformId(stream.ReadUInt32()) | 28u -> Decoration.SaturatedConversion | 29u -> Decoration.Stream(stream.ReadUInt32()) | 30u -> Decoration.Location(stream.ReadUInt32()) | 31u -> Decoration.Component(stream.ReadUInt32()) | 32u -> Decoration.Index(stream.ReadUInt32()) | 33u -> Decoration.Binding(stream.ReadUInt32()) | 34u -> Decoration.DescriptorSet(stream.ReadUInt32()) | 35u -> Decoration.Offset(stream.ReadUInt32()) | 36u -> Decoration.XfbBuffer(stream.ReadUInt32()) | 37u -> Decoration.XfbStride(stream.ReadUInt32()) | 38u -> Decoration.FuncParamAttr(stream.ReadEnum()) | 39u -> Decoration.FPRoundingMode(stream.ReadEnum()) | 40u -> Decoration.FPFastMathMode(stream.ReadEnum()) | 41u -> Decoration.LinkageAttributes(stream.ReadString(), stream.ReadEnum()) | 42u -> Decoration.NoContraction | 43u -> Decoration.InputAttachmentIndex(stream.ReadUInt32()) | 44u -> Decoration.Alignment(stream.ReadUInt32()) | 45u -> Decoration.MaxByteOffset(stream.ReadUInt32()) | 46u -> Decoration.AlignmentId(stream.ReadUInt32()) | 47u -> Decoration.MaxByteOffsetId(stream.ReadUInt32()) | 4469u -> Decoration.NoSignedWrap | 4470u -> Decoration.NoUnsignedWrap | 4999u -> Decoration.ExplicitInterpAMD | 5248u -> Decoration.OverrideCoverageNV | 5250u -> Decoration.PassthroughNV | 5252u -> Decoration.ViewportRelativeNV | 5256u -> Decoration.SecondaryViewportRelativeNV(stream.ReadUInt32()) | 5271u -> Decoration.PerPrimitiveNV | 5272u -> Decoration.PerViewNV | 5273u -> Decoration.PerTaskNV | 5285u -> Decoration.PerVertexNV | 5300u -> Decoration.NonUniform | 5355u -> Decoration.RestrictPointer | 5356u -> Decoration.AliasedPointer | 5634u -> Decoration.CounterBuffer(stream.ReadUInt32()) | 5635u -> Decoration.UserSemantic(stream.ReadString()) | 5636u -> Decoration.UserTypeGOOGLE(stream.ReadString()) | _ -> failwith "invalid" )
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
            OpSubgroupAvcSicGetInterRawSadsINTEL(stream.ReadUInt32(), stream.ReadUInt32(), stream.ReadUInt32())        | _ -> failwith "invalid opcode" 