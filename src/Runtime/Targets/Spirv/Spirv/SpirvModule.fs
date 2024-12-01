[<AutoOpen>]
module Spirv.SpirvModule

open System.IO
open Spirv.InternalHelpers

[<AutoOpen>]
module private Helpers =

    let readInstruction (s: SpirvStream) =
        let opcode = s.ReadUInt16 ()
        s.remaining <- (s.ReadUInt16 () |> int) - 1

        // TODO: This is horrible, what is it about this instruction where we need to subtract by an additional 2?
        match opcode with
        | 245us (* OpPhi *) ->
            s.remaining <- s.remaining - 2
        | _ ->
            ()

        Instruction.Deserialize(opcode, s)

    let writeInstruction (instr: Instruction, s: SpirvStream) =
        let pos = s.Position
        s.WriteUInt16(instr.Opcode)
        let posWordCount = s.Position
        s.WriteUInt16(instr.Opcode)

        Instruction.Serialize(instr, s)

        let endPos = s.Position

        let bytesRead = int (endPos - pos)

        if bytesRead % sizeof<uint32> <> 0 then
            failwithf "Not divisible by %i." sizeof<uint32>

        let wordCount = bytesRead / sizeof<uint32>

        s.Seek(posWordCount, SeekOrigin.Begin)
        s.WriteUInt16(uint16 wordCount)
        s.Seek(endPos, SeekOrigin.Begin)

[<NoEquality;NoComparison>]
type SpirvModule = 
    internal {
        magicNumber: uint32
        versionNumber: uint32
        genMagicNumber: uint32
        bound: uint32
        instrs: Instruction list
    }

    static member CreateVersion(major: uint32, minor: uint32) = 
        ((uint32(byte(major)) <<< 16) ||| (uint32(byte(minor)) <<< 8))

    static member ExtractVersion(version: uint32) : struct(uint32 * uint32) =
        struct(uint32(byte(version >>> 16)), uint32(byte(version >>> 8)))

    static member Create (?version: uint32, ?bound: uint32, ?instrs) =
        let version = defaultArg version 65536u
        let bound = defaultArg bound 65536u
        let instrs = defaultArg instrs []

        {
            magicNumber = MagicNumber
            versionNumber = version
            genMagicNumber = 0u
            bound = bound
            instrs = instrs
        }

    member x.Instructions = x.instrs

    member x.AddInstructions instrs =
        { x with instrs = x.instrs @ instrs }

    static member Deserialize(stream: Stream) =
        let spirvStream =
            {
                stream = stream
                remaining = 0
                buffer128 = Array.zeroCreate 128
            }

        let magicNumber = spirvStream.ReadUInt32 ()
        let versionNumber = spirvStream.ReadUInt32 ()
        let genMagicNumber = spirvStream.ReadUInt32 ()
        let bound = spirvStream.ReadUInt32 ()
        let _reserved = spirvStream.ReadUInt32 ()

        let instrs =
            let xs = ResizeArray ()
            while stream.Position < stream.Length do
                xs.Add(readInstruction spirvStream)
            xs |> List.ofSeq

        {
            magicNumber = magicNumber
            versionNumber = versionNumber
            genMagicNumber = genMagicNumber
            bound = bound
            instrs = instrs
        }

    static member Serialize(stream: Stream, spirvModule: SpirvModule) =
        let spirvStream =
            {
                stream = stream
                remaining = 0
                buffer128 = Array.zeroCreate 128
            }

        spirvStream.WriteUInt32(spirvModule.magicNumber)
        spirvStream.WriteUInt32(spirvModule.versionNumber)
        spirvStream.WriteUInt32(spirvModule.genMagicNumber)
        spirvStream.WriteUInt32(spirvModule.bound)
        spirvStream.WriteUInt32(0u) // reserved

        for instr in spirvModule.instrs do
            writeInstruction (instr, spirvStream)
