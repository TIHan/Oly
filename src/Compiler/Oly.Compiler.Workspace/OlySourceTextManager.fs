namespace Oly.Compiler.Workspace

open System.Collections.Immutable

open Oly.Core
open Oly.Compiler.Text

[<Sealed>]
type OlySourceTextManager private (openedTexts: ImmutableDictionary<OlyPath, IOlySourceText>) =

    static let empty = OlySourceTextManager(ImmutableDictionary.Empty)
    static member Empty = empty

    member this.OnOpen(path: OlyPath, sourceText: IOlySourceText) =
        this.Set(path, sourceText)

    member _.OnClose(path: OlyPath) =
        OlySourceTextManager(openedTexts.Remove(path))

    member this.OnChange(path: OlyPath, textChanges: OlyTextChangeWithRange seq) =
        match openedTexts.TryGetValue(path) with
        | true, sourceText ->
            let newSourceText = sourceText.ApplyTextChanges(textChanges)
            Some(newSourceText, this.Set(path, newSourceText))
        | _ ->
            None

    member _.TryGet(path: OlyPath) =
        match openedTexts.TryGetValue(path) with
        | true, sourceText -> Some sourceText
        | _ -> None

    member _.Set(path, sourceText) =
        OlySourceTextManager(openedTexts.SetItem(path, sourceText))
