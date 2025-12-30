open System
open System.IO
open System.Xml
open System.Text
open System.Linq

[<Literal>]
let InputFilePath = "Syntax.xml"

[<Literal>]
let InternalOutputFilePath = "../../../../Oly.Compiler.Syntax/Internal/Generated.fs"

[<Literal>]
let PublicOutputFilePath = "../../../../Oly.Compiler.Syntax/Generated.fs"


[<NoComparison;NoEquality>]
type cenv =
    {
        doc: XmlDocument
        internalBuilder: StringBuilder
        publicBuilder: StringBuilder
        tags: System.Collections.Generic.List<string>
    }

let findTree cenv =
    let element = cenv.doc.DocumentElement
    if element.Name <> "Tree" then
        failwith "Expected Tree"
    element

let addInternalTag cenv (name: string) =
    cenv.tags.Add(name)

let getInternalTag cenv = cenv.tags.Count
let addInternal cenv (str: string) = cenv.internalBuilder.Append(str) |> ignore
let add cenv (str: string) = cenv.publicBuilder.Append(str) |> ignore

let computeInternalNode cenv (node: XmlNode) =
    let name = node.Attributes.["name"].Value
    $"[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]\ntype {name} =\n"
    |> addInternal cenv

    for case in node.ChildNodes do
        if case.Name <> "Case" then
            failwith "Expected Case"

        let fields = ResizeArray()
        for field in case.ChildNodes do
            if field.Name <> "Field" then
                failwith "Expected Field"

            if String.IsNullOrWhiteSpace(field.InnerText) then
                failwith "Expected Field Type"

            let fieldName = field.Attributes.["name"].Value
            let fieldTyName = field.InnerText
            $"{fieldName}: {fieldTyName}"
            |> fields.Add
            
        if fields.Count > 1 then
            fields.Add("fullWidth: int")

        let caseName = case.Attributes.["name"].Value
        $"    | {caseName}\n"
        |> addInternal cenv

        if fields.Count > 0 then
            "        of\n"
            |> addInternal cenv

            for i = 0 to fields.Count - 1 do
                let field = fields.[i]

                $"        {field}"
                |> addInternal cenv

                if i < fields.Count - 1 then
                    " *\n"
                else
                    "\n"
                |> addInternal cenv
        else
            "        of unit\n"
            |> addInternal cenv

    addInternal cenv "\n"

    let textIsToken = if name = "SyntaxToken" then "true" else "false"

    let mutable hasError = false

    let textGetSlot =
        let builder = StringBuilder()
        let add (str: string) = builder.Append str |> ignore

        $"            match this with\n"
        |> add

        if name = "SyntaxToken" then
            $"            | _ -> failwith \"invalid slot\"\n"
            |> add

            builder.ToString()
        else

        for case in node.ChildNodes do
            let fields = ResizeArray()
            for field in case.ChildNodes do
                let fieldName = field.Attributes.["name"].Value
                fields.Add fieldName 

            let caseName = case.Attributes.["name"].Value
            if caseName = "Error" then
                hasError <- true

            if fields.Count > 0 then

                let textFields =
                    fields
                    |> String.concat ", "

                let textFields =
                    if fields.Count > 1 then
                        textFields + ", _"
                    else
                        textFields

                $"            | {caseName}({textFields}) ->\n"
                |> add

                $"                match index with\n"
                |> add

                for i = 0 to fields.Count - 1 do
                    $"                | {i} -> {fields.[i]} :> ISyntaxNode\n"
                    |> add


                $"                | _ -> failwith \"invalid slot\"\n"
                |> add
            else
                $"            | {caseName} _ ->\n"
                |> add

                $"                failwith \"invalid slot\"\n"
                |> add

        builder.ToString()
        
    let textSlotCount =
        let builder = StringBuilder()
        let add (str: string) = builder.Append str |> ignore

        $"            match this with\n"
        |> add

        if name = "SyntaxToken" then
            $"            | _ -> 0\n"
            |> add

            builder.ToString()
        else

        for case in node.ChildNodes do
            let fieldCount = case.ChildNodes.Count
            let caseName = case.Attributes.["name"].Value
            $"            | {caseName} _ -> {fieldCount}\n"
            |> add

        builder.ToString()   

    let textIsError =
        if hasError then
            $"match this with | Error _ -> true | _ -> false"
        else
            "false"

    let textFullWidth =
        let builder = StringBuilder()
        let add (str: string) = builder.Append str |> ignore

        $"            match this with\n"
        |> add

        if name = "SyntaxToken" then
            $"            | _ -> failwith \"invalid slot\"\n"
            |> add

            builder.ToString()
        else

        for case in node.ChildNodes do
            let fields = ResizeArray()
            for field in case.ChildNodes do
                let fieldName = field.Attributes.["name"].Value
                fields.Add fieldName 

            let caseName = case.Attributes.["name"].Value
            if caseName = "Error" then
                hasError <- true

            if fields.Count > 0 then

                let textFields =
                    if fields.Count = 0 then
                        "_"
                    elif fields.Count > 1 then
                        "fullWidth=fullWidth"
                    else
                        "x"

                $"            | {caseName}({textFields}) ->\n"
                |> add

                if fields.Count = 0 then
                    $"                0\n"
                    |> add
                elif fields.Count > 1 then
                    $"                fullWidth\n"
                    |> add
                else
                    $"                (x :> ISyntaxNode).FullWidth\n"
                    |> add
            else
                $"            | {caseName} _ ->\n"
                |> add

                $"                0\n"
                |> add

        builder.ToString()

    addInternal cenv $"    interface ISyntaxNode with\n\n"
    addInternal cenv $"        member this.IsTerminal = false\n\n"
    addInternal cenv $"        member this.IsToken = {textIsToken}\n\n"
    addInternal cenv $"        member this.IsError = {textIsError}\n\n"
    addInternal cenv $"        member this.GetSlot(index) =\n{textGetSlot}\n"
    addInternal cenv $"        member this.SlotCount =\n{textSlotCount}\n"
    addInternal cenv $"        member this.FullWidth =\n{textFullWidth}\n"
    addInternal cenv $"        member _.Tag = {getInternalTag cenv}\n"
    addInternal cenv $"        member _.InnerTag = Tags.Terminal\n"
    addInternal cenv $"        static member StaticTag = {getInternalTag cenv}\n"
    addInternal cenv $"        static member StaticInnerTag = Tags.Terminal\n\n"

    addInternal cenv "[<RequireQualifiedAccess>]\n"
    addInternal cenv $"module {name} =\n\n"
    addInternal cenv $"    [<Literal>]\n"
    addInternal cenv $"    let Tag = {getInternalTag cenv}\n\n"

    addInternalTag cenv name

let computePublicNode cenv (node: XmlNode) =
    let name = node.Attributes.["name"].Value
    $"[<Sealed;NoComparison>]\ntype Oly{name} internal (tree, start: int, parent, internalNode: {name}) as this =\n    inherit OlySyntaxNode(tree, parent, internalNode)\n\n"
    |> add cenv

    $"""    
    let mutable children: OlySyntaxNode imarray = ImArray.empty
    let mutable textSpan = Unchecked.defaultof<OlyTextSpan>

    member private this.FullWidth =
#if DEBUG || CHECKED
        let fullWidth = (internalNode :> ISyntaxNode).FullWidth
        this.Children
        |> ImArray.iteri (fun i x ->
            OlyAssert.Equal(x.FullTextSpan.Width, (internalNode :> ISyntaxNode).GetSlot(i).FullWidth)
        )
        fullWidth
#else
        (internalNode :> ISyntaxNode).FullWidth
#endif

    override this.TextSpan =
        if textSpan.Start = 0 && textSpan.Width = 0 then
            let offset = (match OlySyntaxNode.TryGetFirstToken(this.Children) with null -> start | x -> x.TextSpan.Start) - start
            textSpan <- if this.Children.IsEmpty then OlyTextSpan.Create(start, 0) else OlyTextSpan.Create(start + offset, this.FullWidth - offset)
        textSpan

    override this.FullTextSpan =
        OlyTextSpan.Create(start, this.FullWidth)

    override _.Children =
        if children.IsEmpty && (internalNode :> ISyntaxNode).SlotCount > 0 then
            children <-
                let mutable p = start
                ImArray.init 
                    (internalNode :> ISyntaxNode).SlotCount 
                    (fun i -> 
                        let slot = (internalNode :> ISyntaxNode).GetSlot(i)
                        let t = Convert.From(tree, p, this, slot)
                        p <- p + t.FullTextSpan.Width
                        t
                    )
        children
    
    member internal _.Internal = internalNode

"""
    |> add cenv

    $"[<RequireQualifiedAccess>]\nmodule Oly{name} =\n"
    |> add cenv

    for case in node.ChildNodes do
        let fields = ResizeArray()
        for field in case.ChildNodes do
            let fieldName = field.Attributes.["name"].Value
            let fieldTyName = 
                field.InnerText
                 .Replace("SyntaxSeparatorList", "OlySyntaxSeparatorList")
                 .Replace("SyntaxList", "OlySyntaxList")
                 .Replace("SyntaxBrackets", "OlySyntaxBrackets")
                 .Replace("SyntaxBracketInnerPipes", "OlySyntaxBracketInnerPipes")
                 .Replace("SyntaxCurlyBrackets", "OlySyntaxCurlyBrackets")
            fields.Add(fieldName, fieldTyName)               

        let textReturnTy =
            let builder = StringBuilder()
            let add (str: string) = builder.Append(str) |> ignore
            let textFields = fields |> Seq.map (fun (fieldName, fieldTyName) -> $"Oly{fieldTyName}") |> String.concat " * "
            if fields.Count > 0 then
                "("
                |> add
                $" {textFields} "
                |> add
                ")"
                |> add
            else
                "unit"
                |> add
            builder.ToString()

        let caseName = case.Attributes.["name"].Value
        $"\n    let (|{caseName}|_|) (node: Oly{name}) : {textReturnTy} option =\n"
        |> add cenv

        if fields.Count > 0 then
            let textConstruct =
                let builder = StringBuilder()
                let add (str: string) = builder.Append(str) |> ignore
                if fields.Count > 0 then
                    "Option.Some ("
                    |> add

                    for i = 0 to fields.Count - 1 do
                        $"System.Runtime.CompilerServices.Unsafe.As node.Children[{i}]"
                        |> add                        
                        if i < fields.Count - 1 then
                            ", "
                            |> add

                    ")"
                    |> add
                else
                    "unit"
                    |> add
                builder.ToString()
                
            $"        match node.Internal with\n"
            |> add cenv

            $"        | {name}.{caseName} _ ->\n"
            |> add cenv

            $"            {textConstruct}\n"
            |> add cenv
        else
            $"        match node.Internal with\n"
            |> add cenv

            $"        | {name}.{caseName} _ ->\n"
            |> add cenv

            $"            Option.Some()\n"
            |> add cenv

        if node.ChildNodes.Count > 1 then
            $"        | _ ->\n"
            |> add cenv

            $"            Option.None\n"
            |> add cenv

    add cenv "\n"

let computeNode cenv (node: XmlNode) =
    computeInternalNode cenv node
    computePublicNode cenv node

let computeTree cenv (tree: XmlElement) =
    for node in tree.ChildNodes do
        if node.Name <> "Node" then
            failwith "Expected Node"
        computeNode cenv node

let computeConversionTree cenv (tree: XmlElement) =
    $"[<RequireQualifiedAccess>]\nmodule private Convert =\n"
    |> add cenv

    $"    let convert = From\n\n"
    |> add cenv

    $"    let From(tree: OlySyntaxTree, start: int, parent: OlySyntaxNode, internalNode: ISyntaxNode) : OlySyntaxNode =\n"
    |> add cenv

    "        match internalNode.Tag with\n"
    |> add cenv

    "        | Tags.Token -> OlySyntaxToken(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) : OlySyntaxNode\n"
    |> add cenv

    "        | Tags.Terminal -> tree.DummyNode\n"
    |> add cenv

    cenv.tags
    |> Seq.iter (fun name ->
        $"        | {name}.Tag -> Oly{name}(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) : OlySyntaxNode\n"
        |> add cenv
    )

    "        | _ ->\n\n"
    |> add cenv

    "        match internalNode with\n"
    |> add cenv

    // HACKY

    $"        | :? SyntaxBrackets<SyntaxSeparatorList<SyntaxType>> as internalNode -> OlySyntaxBrackets<OlySyntaxSeparatorList<OlySyntaxType>>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBracketInnerPipes<SyntaxSeparatorList<SyntaxType>> as internalNode -> OlySyntaxBracketInnerPipes<OlySyntaxSeparatorList<OlySyntaxType>>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBracketInnerPipes<SyntaxSeparatorList<SyntaxExpression>> as internalNode -> OlySyntaxBracketInnerPipes<OlySyntaxSeparatorList<OlySyntaxExpression>>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxCurlyBrackets<SyntaxSeparatorList<SyntaxType>> as internalNode -> OlySyntaxCurlyBrackets<OlySyntaxSeparatorList<OlySyntaxType>>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxCurlyBrackets<SyntaxSeparatorList<SyntaxExpression>> as internalNode -> OlySyntaxCurlyBrackets<OlySyntaxSeparatorList<OlySyntaxExpression>>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxConstraintClause> as internalNode -> OlySyntaxSeparatorList<OlySyntaxConstraintClause>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxConstraintClause> as internalNode -> OlySyntaxList<OlySyntaxConstraintClause>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxParameter> as internalNode -> OlySyntaxSeparatorList<OlySyntaxParameter>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxTupleElement> as internalNode -> OlySyntaxSeparatorList<OlySyntaxTupleElement>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxParameter> as internalNode -> OlySyntaxList<OlySyntaxParameter>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxExpression> as internalNode -> OlySyntaxSeparatorList<OlySyntaxExpression>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxExpression> as internalNode -> OlySyntaxList<OlySyntaxExpression>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxAttribute> as internalNode -> OlySyntaxSeparatorList<OlySyntaxAttribute>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxAttribute> as internalNode -> OlySyntaxList<OlySyntaxAttribute>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxFieldPattern> as internalNode -> OlySyntaxSeparatorList<OlySyntaxFieldPattern>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxFieldPattern> as internalNode -> OlySyntaxList<OlySyntaxFieldPattern>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxConstraint> as internalNode -> OlySyntaxSeparatorList<OlySyntaxConstraint>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxConstraint> as internalNode -> OlySyntaxList<OlySyntaxConstraint>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxType> as internalNode -> OlySyntaxSeparatorList<OlySyntaxType>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxType> as internalNode -> OlySyntaxList<OlySyntaxType>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxMatchClause> as internalNode -> OlySyntaxSeparatorList<OlySyntaxMatchClause>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxMatchClause> as internalNode -> OlySyntaxList<OlySyntaxMatchClause>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxMatchPattern> as internalNode -> OlySyntaxSeparatorList<OlySyntaxMatchPattern>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxMatchPattern> as internalNode -> OlySyntaxList<OlySyntaxMatchPattern>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxBinding> as internalNode -> OlySyntaxSeparatorList<OlySyntaxBinding>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxBinding> as internalNode -> OlySyntaxList<OlySyntaxBinding>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxPropertyBinding> as internalNode -> OlySyntaxSeparatorList<OlySyntaxPropertyBinding>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxPattern> as internalNode -> OlySyntaxSeparatorList<OlySyntaxPattern>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxPattern> as internalNode -> OlySyntaxList<OlySyntaxPattern>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBrackets<SyntaxAttribute> as internalNode -> OlySyntaxBrackets<OlySyntaxAttribute>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxHashAttribute> as internalNode -> OlySyntaxList<OlySyntaxHashAttribute>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxNamedArgument> as internalNode -> OlySyntaxSeparatorList<OlySyntaxNamedArgument>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBrackets<SyntaxList<SyntaxToken>> as internalNode -> OlySyntaxBrackets<OlySyntaxList<OlySyntaxToken>>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBracketInnerPipes<SyntaxList<SyntaxToken>> as internalNode -> OlySyntaxBracketInnerPipes<OlySyntaxList<OlySyntaxToken>>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxToken> as internalNode -> OlySyntaxList<OlySyntaxToken>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBrackets<SyntaxSeparatorList<SyntaxExpression>> as internalNode -> OlySyntaxBrackets<OlySyntaxSeparatorList<OlySyntaxExpression>>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxTypeDeclarationCase> as internalNode -> OlySyntaxList<OlySyntaxTypeDeclarationCase>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxValueDeclarationPremodifier> as internalNode -> OlySyntaxList<OlySyntaxValueDeclarationPremodifier>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxValueDeclarationPostmodifier> as internalNode -> OlySyntaxList<OlySyntaxValueDeclarationPostmodifier>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBrackets<SyntaxSeparatorList<SyntaxAttribute>> as internalNode -> OlySyntaxBrackets<OlySyntaxList<OlySyntaxValueDeclarationPostmodifier>>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBrackets<SyntaxFixedArrayLength> as internalNode -> OlySyntaxBrackets<OlySyntaxFixedArrayLength>(tree, start, parent, internalNode, convert) : OlySyntaxNode\n"
    |> add cenv

    // END HACKY

    $"        | _ -> failwith \"Invalid Internal Syntax Node\"\n"
    |> add cenv

let generate() =
    let doc = XmlDocument()
    doc.Load(InputFilePath)
    let cenv =
        {
            doc = doc
            internalBuilder = StringBuilder()
            publicBuilder = StringBuilder()
            tags = System.Collections.Generic.List()
        }

    addInternal cenv "// Generated File Do Not Modify\n"
    addInternal cenv "[<AutoOpen>]\nmodule internal rec Oly.Compiler.Syntax.Internal.Generated\n\n"
    addInternal cenv "#nowarn \"3535\"\n\n"
    addInternal cenv "#nowarn \"3536\"\n\n"

    add cenv "// Generated File Do Not Modify\n"
    add cenv "namespace rec Oly.Compiler.Syntax\n\n"
    add cenv "open Oly.Core\n"
    add cenv "open Oly.Compiler.Text\n"
    add cenv "open Oly.Compiler.Syntax.Internal\n\n"
    add cenv "#nowarn \"40\"\n\n"
    add cenv "#nowarn \"3535\"\n\n"
    add cenv "#nowarn \"3536\"\n\n"

    let tree = findTree cenv
    computeTree cenv tree
    computeConversionTree cenv tree

    File.WriteAllText(InternalOutputFilePath, cenv.internalBuilder.ToString())
    File.WriteAllText(PublicOutputFilePath, cenv.publicBuilder.ToString())

[<EntryPoint>]
let main _ =
    generate()
    0