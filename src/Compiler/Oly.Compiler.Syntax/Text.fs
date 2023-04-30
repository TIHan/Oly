module Oly.Compiler.Text

open System
open System.IO
open FSharp.NativeInterop

#nowarn "9"

[<Struct>]
type OlyTextSpan private (start: int, width: int) =

    member _.Start = start

    member _.Width = width

    member this.End = start + width

    member this.Contains(position: int) =
        position >= this.Start && position < this.End && width <> 0

    member this.Contains(textSpan: OlyTextSpan) =
        textSpan.Start >= this.Start && textSpan.End <= this.End && width <> 0

    member this.IntersectsWith(position: int) =
        position >= this.Start && position <= this.End

    member this.IntersectsWith(textSpan: OlyTextSpan) =
        (textSpan.Start >= this.Start && textSpan.End <= this.End) ||
        (this.Start >= textSpan.Start && this.End <= textSpan.End)

    member this.IsEqualTo(textSpan: OlyTextSpan) =
        this.Start = textSpan.Start && this.Width = textSpan.Width

    static member Create(start, width) =
        if start < 0 then invalidArg "start" "Less than zero."
        if width < 0 then invalidArg "width" "Less than zero."
        OlyTextSpan(start, width)

    static member CreateWithEnd(start: int, ending: int) =
        if start < 0 then invalidArg "start" "Less than zero."
        if ending < start then invalidArg "en" "Less than 'start'."
        OlyTextSpan(start, ending - start)

[<Struct>]
type OlyTextPosition =

    val mutable Line: int

    val mutable Column: int

    new(line, column) = { Line = line; Column = column }

[<Struct>]
type OlyTextRange =

    val mutable Start: OlyTextPosition

    val mutable End: OlyTextPosition

    new(startPos, endPos) = { Start = startPos; End = endPos }

    member this.Combine(textRange: OlyTextRange) =
        let startLine =
            if this.Start.Line <= textRange.Start.Line then this.Start.Line
            else textRange.Start.Line

        let startColumn =
            if this.Start.Column <= textRange.Start.Column then this.Start.Column
            else textRange.Start.Column

        let endLine =
            if this.End.Line >= textRange.End.Line then this.End.Line
            else textRange.End.Line

        let endColumn =
            if this.End.Column >= textRange.End.Column then this.End.Column
            else textRange.End.Column

        OlyTextRange(OlyTextPosition(startLine, startColumn), OlyTextPosition(endLine, endColumn))

[<Struct>]
type OlyTextChange =

    val mutable Span: OlyTextSpan

    val mutable Text: string

    new(span, text) = { Span = span; Text = text }

[<Struct>]
type OlyTextChangeWithRange =

    val mutable Range: OlyTextRange

    val mutable Text: string

    new(range, text) = { Range = range; Text = text }

type IOlySourceText =

    abstract Item : int -> char with get

    abstract GetSubText : textSpan: OlyTextSpan -> IOlySourceText

    abstract GetSubTextView : start: int * length: int -> ReadOnlyMemory<char>

    abstract SubContentEquals : target: string * startIndex: int -> bool

    abstract SubContentEquals : target: Memory<char> * startIndex: int -> bool

    abstract Length : int

    abstract ContentEquals : sourceText: IOlySourceText -> bool

    abstract CopyTo : sourceIndex: int * destination: Span<char> -> unit

    abstract CopyTo : sourceIndex: int * destination: char [] * destinationIndex: int * count: int -> unit

    abstract GetTextRange : OlyTextSpan -> OlyTextRange

    abstract TryGetPosition : textPos: OlyTextPosition -> int option

    abstract TryGetTextSpan : textRange: OlyTextRange -> OlyTextSpan option

    abstract ApplyTextChanges : textChanges: OlyTextChange seq -> IOlySourceText

    abstract Lines : OlySourceTextLineCollection

and [<AbstractClass>] OlySourceTextLineCollection internal () =

    abstract Item : index: int -> OlySourceTextLine

    abstract Count : int

    abstract GetLineFromPosition : position: int -> OlySourceTextLine

and [<Struct;NoComparison;NoEquality>] OlySourceTextLine internal (textSpan: OlyTextSpan, lineIndex: int, sourceText: IOlySourceText) =

    member _.TextSpan = textSpan

    member _.SourceText = sourceText

    member _.LineIndex = lineIndex

    override _.ToString() = 
        let mutable str = String.init textSpan.Width (fun _ -> string Char.MinValue)
        use ptr = fixed str
        let span = Span(ptr |> NativePtr.toVoidPtr, textSpan.Width)
        sourceText.CopyTo(textSpan.Start, span)
        str       

[<AutoOpen>]
module OlySourceTextExtensions =

    type IOlySourceText with

        /// Will throw.
        member this.GetTextSpan(textRange: OlyTextRange) =
            match this.TryGetTextSpan(textRange) with
            | Some textSpan -> textSpan
            | _ -> invalidArg (nameof(textRange)) "Invalid range for source text."

        member this.ApplyTextChanges(textChanges: OlyTextChangeWithRange seq) =
            let textChanges =
                textChanges
                |> Seq.map (fun x ->
                    OlyTextChange(
                        this.GetTextSpan(x.Range),
                        x.Text
                    )
                )
            this.ApplyTextChanges(textChanges)

module private Helpers =

    let inline tryBinaryFind ([<InlineIfLambda>] comparer: 'a -> 'b -> int) (value: 'a) (source: 'b[]) : int =
        let rec loop lo hi =
            if lo > hi then -1
            else
                let mid = lo + (hi - lo) / 2
                match sign (comparer value source[mid]) with
                | 0 -> mid
                | 1 -> loop (mid + 1) hi
                | _ -> loop lo (mid - 1)

        loop 0 (source.Length - 1)

[<Sealed>]
type private StringText(str: string) as this =

    let getLines =
        lazy
            StringTextLineCollection(this)

    let hashCode =
        lazy
            str.GetHashCode()

    member __.String = str

    override __.GetHashCode() = hashCode.Value
    override __.Equals(obj: obj) = 
        match obj with
        | :? StringText as other -> other.String.Equals(str)
        | :? string as other -> other.Equals(str)
        | _ -> false        
    override __.ToString() = str

    interface IOlySourceText with

        member this.ApplyTextChanges(textChanges) =           
            let appliedText =
                (str, textChanges)
                ||> Seq.fold (fun str textChange ->
                    let textSpan = textChange.Span
                    let text = textChange.Text
                    if textSpan.Width > 0 then
                        str.Remove(textSpan.Start, textSpan.Width).Insert(textSpan.Start, text)
                    else
                        str.Insert(textSpan.Start, text)
                )
            StringText(appliedText) :> IOlySourceText

        member _.Item with get index = str.[index]

        member _.GetSubText(textSpan) =
            str.Substring(textSpan.Start, textSpan.Width)
            |> StringText
            :> IOlySourceText

        member this.GetSubTextView(start, length) =
            str.AsMemory(start, length)

        member _.SubContentEquals(target, startIndex) =
            if startIndex < 0 || startIndex >= str.Length then
                invalidArg "startIndex" "Out of range."

            if String.IsNullOrEmpty(target) then
                invalidArg "target" "Is null or empty."

            let lastIndex = startIndex + target.Length
            if lastIndex <= startIndex || lastIndex >= str.Length then
                invalidArg "target" "Too big."

            str.IndexOf(target, startIndex, target.Length) <> -1            
            
        member _.SubContentEquals(target: Memory<char>, startIndex) =
            if startIndex < 0 || startIndex >= str.Length then
                invalidArg "startIndex" "Out of range."

            if target.Length = 0 then
                invalidArg "target" "Is null or empty."

            let lastIndex = startIndex + target.Length
            if lastIndex <= startIndex || lastIndex >= str.Length then
                invalidArg "target" "Too big."

            str.AsSpan(startIndex).IndexOf(target.Span) <> -1  

        member _.Length = str.Length

        member this.ContentEquals(sourceText) =
            match sourceText with
            | :? StringText as sourceText when sourceText = this || sourceText.String = str -> true
            | _ -> false

        member _.CopyTo(sourceIndex, span) =
            str.AsSpan().Slice(sourceIndex, span.Length).CopyTo(span)

        member this.CopyTo(sourceIndex: int, destination: char [], destinationIndex, count) : unit =
            let span = Span(destination, destinationIndex, count)
            (this :> IOlySourceText).CopyTo(sourceIndex, span)

        member _.GetTextRange(span: OlyTextSpan) =
            let lines = getLines.Value
            let startPos = span.Start
            let endPos = startPos + span.Width

            let line1 = lines.GetLineFromPosition(startPos)
            let line2 = lines.GetLineFromPosition(endPos)

            OlyTextRange(OlyTextPosition(line1.LineIndex, startPos - line1.TextSpan.Start), OlyTextPosition(line2.LineIndex, endPos - line2.TextSpan.Start))

        member _.TryGetPosition(textPos) =
            let line = textPos.Line
            let column = textPos.Column
            let lines = getLines.Value
            if line >= lines.Count || line < 0 then
                None
            else
                let line = lines.[line]
                let position = line.TextSpan.Start + column
                if position <= line.TextSpan.End then
                    Some position
                else
                    None

        member this.TryGetTextSpan(textRange) =
            match (this :> IOlySourceText).TryGetPosition(textRange.Start), (this :> IOlySourceText).TryGetPosition(textRange.End) with
            | Some(pos1), Some(pos2) ->
                OlyTextSpan.Create(pos1, pos2 - pos1) |> Some
            | _ ->
                None

        member this.Lines = getLines.Value :> OlySourceTextLineCollection

and [<Sealed>] private StringTextLineCollection(sourceText: StringText) =
    inherit OlySourceTextLineCollection()

    let lines = 
        [|
            let mutable startPos = OlyTextPosition()
            let mutable endPos = OlyTextPosition()
            let mutable line = 0
            let mutable col = 0
            let mutable pos = 0

            for c in sourceText.String do
                match c with
                | '\n' -> 
                    yield OlySourceTextLine(OlyTextSpan.CreateWithEnd(pos - endPos.Column, pos), line, sourceText)
                    line <- line + 1
                    col <- 0
                    startPos <- OlyTextPosition(line, col)
                    endPos <- startPos
                | _ ->
                    col <- col + 1
                    endPos <- OlyTextPosition(line, col)

                pos <- pos + 1

            yield OlySourceTextLine(OlyTextSpan.CreateWithEnd(pos - endPos.Column, pos), line, sourceText)
        |]

    override _.Item index = lines.[index]

    override _.Count = lines.Length

    override _.GetLineFromPosition(pos: int) =
        let index =
            (pos, lines)
            ||> Helpers.tryBinaryFind (fun pos line ->
                if line.TextSpan.IntersectsWith(pos) then
                    0
                elif pos < line.TextSpan.Start then
                    -1
                else
                    1
            )
        lines[index]

[<Sealed;AbstractClass>]
type OlySourceText private () =

    static member Create(text: string) =
        StringText(text) :> IOlySourceText

    static member FromFile(filePath: string) =
        StringText(File.ReadAllText(filePath)) :> IOlySourceText
