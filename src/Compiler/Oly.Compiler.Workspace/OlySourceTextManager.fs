namespace Oly.Compiler.Workspace

open System
open System.Collections.Generic
open Oly.Core
open Oly.Compiler.Text

[<Sealed>]
type OlySourceTextManager() =

    let lockObj = obj()
    let openedTexts = Dictionary<OlyPath, IOlySourceText * Nullable<int>>()

    member _.OnOpen(path: OlyPath, version) =
        lock lockObj <| fun _ ->
        let sourceText = OlySourceText.FromFile(path.ToString())
        openedTexts[path] <- sourceText, version

    member _.OnClose(path: OlyPath) =
        lock lockObj <| fun _ ->
        match openedTexts.Remove(path) with
        | _ -> ()

    member _.OnChange(path: OlyPath, version, textChanges: OlyTextChangeWithRange seq) =
        lock lockObj <| fun _ ->
        match openedTexts.TryGetValue(path) with
        | true, (sourceText, _) ->
            let newSourceText = sourceText.ApplyTextChanges(textChanges)
            openedTexts[path] <- newSourceText, version
            Some(newSourceText)
        | _ ->
            None

    member _.TryGet(path: OlyPath) =
        lock lockObj <| fun _ ->
        match openedTexts.TryGetValue(path) with
        | true, sourceText -> Some sourceText
        | _ -> None

    member _.TrySet(path, sourceText) =
        lock lockObj <| fun _ ->
        match openedTexts.TryGetValue(path) with
        | true, _ -> 
            openedTexts[path] <- sourceText
            true
        | _ ->
            false