module internal Spirv.InternalHelpers

open System
open System.IO

[<RequireQualifiedAccess>]
module private LittleEndian =

    let inline write8 (data: Span<byte>) offset value =
        data.[offset] <- byte value

    let inline write16 (data: Span<byte>) offset value =
        data.[offset] <- byte value
        data.[offset + 1] <- byte (value >>> 8)

    let inline write32 (data: Span<byte>) offset value =
        data.[offset] <- byte value
        data.[offset + 1] <- byte (value >>> 8)
        data.[offset + 2] <- byte (value >>> 16)
        data.[offset + 3] <- byte (value >>> 24)

    let inline write64 (data: Span<byte>) offset value =
        data.[offset] <- byte value
        data.[offset + 1] <- byte (value >>> 8)
        data.[offset + 2] <- byte (value >>> 16)
        data.[offset + 3] <- byte (value >>> 24)
        data.[offset + 4] <- byte (value >>> 32)
        data.[offset + 5] <- byte (value >>> 40)
        data.[offset + 6] <- byte (value >>> 48)
        data.[offset + 7] <- byte (value >>> 56)

    let inline read8 (data: ReadOnlySpan<byte>) offset =
        data.[offset]

    let inline read16 (data: ReadOnlySpan<byte>) offset =
        ( uint16 data.[offset]) |||
        ((uint16 data.[offset + 1]) <<< 8)

    let inline read32 (data: ReadOnlySpan<byte>) offset =
        ( uint32 data.[offset]) |||
        ((uint32 data.[offset + 1]) <<< 8) |||
        ((uint32 data.[offset + 2]) <<< 16) |||
        ((uint32 data.[offset + 3]) <<< 24)

    let inline read64 (data: ReadOnlySpan<byte>) offset =
        ( uint64 data.[offset]) |||
        ((uint64 data.[offset + 1]) <<< 8) |||
        ((uint64 data.[offset + 2]) <<< 16) |||
        ((uint64 data.[offset + 3]) <<< 24) |||
        ((uint64 data.[offset + 4]) <<< 32) |||
        ((uint64 data.[offset + 5]) <<< 40) |||
        ((uint64 data.[offset + 6]) <<< 48) |||
        ((uint64 data.[offset + 7]) <<< 56)

let private wordRemainder n =
    let remainder = n % sizeof<uint32>
    if remainder = 0 then
        sizeof<uint32>
    else
        sizeof<uint32> - remainder

[<NoEquality;NoComparison>]
type SpirvStream =
    {
        stream: Stream
        mutable remaining: int
        buffer128: byte []
    }

    member inline x.ReadOnlyBuffer len = ReadOnlySpan(x.buffer128, 0, len)

    member inline x.Buffer len = Span(x.buffer128, 0, len)

    member inline x.Position = int x.stream.Position

    member inline x.Seek (offset, origin) = x.stream.Seek (int64 offset, origin) |> ignore

    member x.WriteUInt16 (v: uint16) =
        let buf = x.Buffer 2
        LittleEndian.write16 buf 0 v
        x.stream.Write (Span.op_Implicit buf)

    member x.WriteUInt32 (v: uint32) =
        let buf = x.Buffer 4
        LittleEndian.write32 buf 0 v
        x.stream.Write (Span.op_Implicit buf)

    member x.WriteString (v: string) =
        let bytes = Text.UTF8Encoding.UTF8.GetBytes v
        let remainder = wordRemainder bytes.Length

        for i = 0 to remainder - 1 do
            x.buffer128.[i] <- 0uy

        x.stream.Write(bytes, 0, bytes.Length)
        x.stream.Write(x.buffer128, 0, remainder)

    member x.WriteEnum<'T when 'T : enum<uint32>> (v: 'T) =
        x.WriteUInt32 (LanguagePrimitives.EnumToValue v)

    member x.WriteOption (v: 'T option, f: 'T -> unit) =
        v |> Option.iter f

    member x.WriteList (v: 'T list, f: 'T -> unit) =
        v |> List.iter f

    member x.ReadUInt16 () =
        let buf = x.Buffer 2
        x.stream.Read buf |> ignore
        let res = LittleEndian.read16 (Span.op_Implicit buf) 0
        if x.remaining > 0 then
            x.remaining <- x.remaining - 1
        res

    member x.ReadUInt32 () =
        let buf = x.Buffer 4
        x.stream.Read buf |> ignore
        let res = LittleEndian.read32 (Span.op_Implicit buf) 0
        if x.remaining > 0 then
            x.remaining <- x.remaining - 1
        res

    member x.ReadString () =
        let startPos = x.Position
        let mutable length = 0

        while not (x.stream.ReadByte() = 0) do
            length <- length + 1

        x.Seek(startPos, SeekOrigin.Begin)

        let bytes = Array.zeroCreate length
        x.stream.Read(bytes, 0, bytes.Length) |> ignore
        let res = Text.UTF8Encoding.UTF8.GetString(bytes)

        // Padding
        let remainder = wordRemainder bytes.Length
        x.Seek(remainder, SeekOrigin.Current)

        let endPos = x.Position

        let bytesRead = int (endPos - startPos)

        if bytesRead % sizeof<uint32> <> 0 then
            failwithf "Not divisible by %i." sizeof<uint32>

        let wordCount = bytesRead / sizeof<uint32>
        if x.remaining > 0 then
            x.remaining <- x.remaining - wordCount
        res

    member x.ReadEnum<'T when 'T : enum<uint32>> () : 'T =
        LanguagePrimitives.EnumOfValue (x.ReadUInt32 ())

    member x.ReadOption (f: unit -> 'T) =
        if x.remaining > 0 then
            Some (f ())
        else
            None

    member x.ReadList (f: unit -> 'T) =
        List.init x.remaining (fun _ -> f ())