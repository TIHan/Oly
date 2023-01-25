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
    }

let findTree cenv =
    let element = cenv.doc.DocumentElement
    if element.Name <> "Tree" then
        failwith "Expected Tree"
    element

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

let computePublicNode cenv (node: XmlNode) =
    let name = node.Attributes.["name"].Value
    $"[<Sealed;NoComparison>]\ntype Oly{name} internal (tree, start: int, parent, internalNode: {name}) as this =\n    inherit OlySyntaxNode(tree, parent, internalNode)\n\n"
    |> add cenv

    $"""    
    let mutable children: OlySyntaxNode imarray = ImArray.empty
    let mutable textSpan = Unchecked.defaultof<OlyTextSpan>

    member private this.FullWidth =
#if DEBUG
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
                        $"node.Children.[{i}] :?> _"
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


let computeConversionNode cenv (node: XmlNode) =
    let name = node.Attributes.["name"].Value

    $"        | :? {name} as internalNode -> Oly{name}(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

let computeConversionTree cenv (tree: XmlElement) =
    $"[<RequireQualifiedAccess>]\nmodule private Convert =\n"
    |> add cenv

    $"    let From(tree: OlySyntaxTree, start: int, parent: OlySyntaxNode, internalNode: ISyntaxNode) : OlySyntaxNode =\n        match internalNode with\n"
    |> add cenv

    $"        | :? SyntaxToken as internalNode -> OlySyntaxToken(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    // HACKY

    $"        | :? SyntaxBrackets<SyntaxSeparatorList<SyntaxType>> as internalNode -> OlySyntaxBrackets<OlySyntaxSeparatorList<OlySyntaxType>>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBracketInnerPipes<SyntaxSeparatorList<SyntaxType>> as internalNode -> OlySyntaxBracketInnerPipes<OlySyntaxSeparatorList<OlySyntaxType>>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBracketInnerPipes<SyntaxSeparatorList<SyntaxExpression>> as internalNode -> OlySyntaxBracketInnerPipes<OlySyntaxSeparatorList<OlySyntaxExpression>>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxCurlyBrackets<SyntaxSeparatorList<SyntaxType>> as internalNode -> OlySyntaxCurlyBrackets<OlySyntaxSeparatorList<OlySyntaxType>>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxCurlyBrackets<SyntaxSeparatorList<SyntaxExpression>> as internalNode -> OlySyntaxCurlyBrackets<OlySyntaxSeparatorList<OlySyntaxExpression>>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxConstraintClause> as internalNode -> OlySyntaxSeparatorList<OlySyntaxConstraintClause>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxConstraintClause> as internalNode -> OlySyntaxList<OlySyntaxConstraintClause>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxParameter> as internalNode -> OlySyntaxSeparatorList<OlySyntaxParameter>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxTupleElement> as internalNode -> OlySyntaxSeparatorList<OlySyntaxTupleElement>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxParameter> as internalNode -> OlySyntaxList<OlySyntaxParameter>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxExpression> as internalNode -> OlySyntaxSeparatorList<OlySyntaxExpression>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxExpression> as internalNode -> OlySyntaxList<OlySyntaxExpression>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxAttribute> as internalNode -> OlySyntaxSeparatorList<OlySyntaxAttribute>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxAttribute> as internalNode -> OlySyntaxList<OlySyntaxAttribute>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxFieldPattern> as internalNode -> OlySyntaxSeparatorList<OlySyntaxFieldPattern>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxFieldPattern> as internalNode -> OlySyntaxList<OlySyntaxFieldPattern>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxConstraint> as internalNode -> OlySyntaxSeparatorList<OlySyntaxConstraint>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxConstraint> as internalNode -> OlySyntaxList<OlySyntaxConstraint>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxType> as internalNode -> OlySyntaxSeparatorList<OlySyntaxType>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxType> as internalNode -> OlySyntaxList<OlySyntaxType>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxMatchClause> as internalNode -> OlySyntaxSeparatorList<OlySyntaxMatchClause>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxMatchClause> as internalNode -> OlySyntaxList<OlySyntaxMatchClause>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxMatchPattern> as internalNode -> OlySyntaxSeparatorList<OlySyntaxMatchPattern>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxMatchPattern> as internalNode -> OlySyntaxList<OlySyntaxMatchPattern>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxBinding> as internalNode -> OlySyntaxSeparatorList<OlySyntaxBinding>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxBinding> as internalNode -> OlySyntaxList<OlySyntaxBinding>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxPropertyBinding> as internalNode -> OlySyntaxList<OlySyntaxPropertyBinding>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxPattern> as internalNode -> OlySyntaxSeparatorList<OlySyntaxPattern>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxPattern> as internalNode -> OlySyntaxList<OlySyntaxPattern>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBrackets<SyntaxAttribute> as internalNode -> OlySyntaxBrackets<OlySyntaxAttribute>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxHashAttribute> as internalNode -> OlySyntaxList<OlySyntaxHashAttribute>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxSeparatorList<SyntaxNamedArgument> as internalNode -> OlySyntaxSeparatorList<OlySyntaxNamedArgument>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBrackets<SyntaxList<SyntaxToken>> as internalNode -> OlySyntaxBrackets<OlySyntaxList<OlySyntaxToken>>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBracketInnerPipes<SyntaxList<SyntaxToken>> as internalNode -> OlySyntaxBracketInnerPipes<OlySyntaxList<OlySyntaxToken>>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxToken> as internalNode -> OlySyntaxList<OlySyntaxToken>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxBrackets<SyntaxSeparatorList<SyntaxExpression>> as internalNode -> OlySyntaxBrackets<OlySyntaxSeparatorList<OlySyntaxExpression>>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxTypeDeclarationCase> as internalNode -> OlySyntaxList<OlySyntaxTypeDeclarationCase>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxValueDeclarationPremodifier> as internalNode -> OlySyntaxList<OlySyntaxValueDeclarationPremodifier>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    $"        | :? SyntaxList<SyntaxValueDeclarationPostmodifier> as internalNode -> OlySyntaxList<OlySyntaxValueDeclarationPostmodifier>(tree, start, parent, internalNode) :> OlySyntaxNode\n"
    |> add cenv

    // END HACKY

    for node in tree.ChildNodes do
        computeConversionNode cenv node

    $"        | _ -> if obj.ReferenceEquals(syntaxTerminal, internalNode) then tree.DummyNode else failwith \"Invalid Internal Syntax Node\"\n"
    |> add cenv

let generate() =
    let doc = XmlDocument()
    doc.Load(InputFilePath)
    let cenv =
        {
            doc = doc
            internalBuilder = StringBuilder()
            publicBuilder = StringBuilder()
        }

    addInternal cenv "// Generated File Do Not Modify\n"
    addInternal cenv "[<AutoOpen>]\nmodule internal rec Oly.Compiler.Syntax.Internal.Generated\n\n"
    addInternal cenv "open Oly.Core\n\n"

    "
[<AutoOpen>]
module SyntaxHelpers =
    let dummyToken = SyntaxToken.Token(Dummy)

    let syntaxTerminal =
        { new ISyntaxNode with
            member _.IsTerminal = true
            member _.IsToken = false
            member _.IsError = false
            member _.GetSlot _ = failwith \"Internal error: Syntax node does not exist.\"
            member _.SlotCount = 0
            member _.FullWidth = 0
        }

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxSeparatorList<'T when 'T :> ISyntaxNode> =
    | Empty of unit
    | List of head: 'T * separatorToken: SyntaxToken * tail: SyntaxSeparatorList<'T> * fullWidth: int
    | Error

    member this.Count =
        match this with
        | Empty _ -> 0
        | List(_, _, tail, _) ->
            1 + tail.Count
        | Error -> 0

    member this.Values =
        seq {
            match this with
            | Empty _ -> ()
            | List(head, _, tail, _) ->
                yield head
                yield! tail.Values
            | Error -> ()
        }

    member this.TryHead() =
        match this with
        | List(head=head) -> Some head
        | _ -> None

    interface ISyntaxNode with

        member _.IsTerminal = false

        member _.IsToken = false

        member this.IsError = 
            match this with
            | Error -> true
            | _ -> false

        member this.GetSlot index =
            match this with
            | Empty _ ->
                match index with
                | _ -> failwith \"invalid slot\"
            | List(head, separatorToken, tail, _) ->
                match index with
                | 0 -> head :> ISyntaxNode
                | 1 -> separatorToken :> ISyntaxNode
                | _ -> (tail :> ISyntaxNode).GetSlot(index - 2)
            | Error ->
                match index with
                | _ -> failwith \"invalid slot\"

        member this.SlotCount =
            match this with
            | Empty _ -> 0
            | List(_, _, tail, _) -> (tail :> ISyntaxNode).SlotCount + 2
            | Error -> 0

        member this.FullWidth =
            match this with
            | List(fullWidth=fullWidth) -> fullWidth
            | _ -> 0

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxList<'T when 'T :> ISyntaxNode> =
    | Empty of unit
    | List of head: 'T * tail: SyntaxList<'T> * fullWidth: int

    member this.Count =
        match this with
        | Empty _ -> 0
        | List(_, tail, _) ->
            1 + tail.Count

    member this.Values =
        seq {
            match this with
            | Empty _ -> ()
            | List(head, tail, _) ->
                yield head
                yield! tail.Values
        }

    interface ISyntaxNode with

        member _.IsTerminal = false

        member _.IsToken = false

        member _.IsError = false

        member this.GetSlot index =
            match this with
            | Empty _ ->
                match index with
                | _ -> failwith \"invalid slot\"
            | List(head, tail, _) ->
                match index with
                | 0 -> head :> ISyntaxNode
                | _ -> (tail :> ISyntaxNode).GetSlot(index - 1)

        member this.SlotCount =
            match this with
            | Empty _ -> 0
            | List(_, tail, _) -> (tail :> ISyntaxNode).SlotCount + 1

        member this.FullWidth =
            match this with
            | List(fullWidth=fullWidth) -> fullWidth
            | _ -> 0

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxBrackets<'T when 'T :> ISyntaxNode> =
    | Brackets
        of
        leftBracketToken: SyntaxToken *
        element: 'T *
        rightBracketToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with
    
        member this.IsTerminal = false
    
        member this.IsToken = false
    
        member this.IsError = false
    
        member this.GetSlot(index) =
            match this with
            | Brackets(leftBracketToken, element, rightBracketToken, _) ->
                match index with
                | 0 -> leftBracketToken :> ISyntaxNode
                | 1 -> element :> ISyntaxNode
                | 2 -> rightBracketToken :> ISyntaxNode
                | _ -> failwith \"invalid slot\"
    
        member this.SlotCount =
            match this with
            | Brackets _ -> 3

        member this.FullWidth =
            match this with
            | Brackets(fullWidth=fullWidth) -> fullWidth

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxBracketInnerPipes<'T when 'T :> ISyntaxNode> =
    | BracketInnerPipes
        of
        leftBracketInnerPipeToken: SyntaxToken *
        element: 'T *
        rightBracketInnerPipeToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with
    
        member this.IsTerminal = false
    
        member this.IsToken = false
    
        member this.IsError = false
    
        member this.GetSlot(index) =
            match this with
            | BracketInnerPipes(leftBracketInnerPipeToken, element, rightBracketInnerPipeToken, _) ->
                match index with
                | 0 -> leftBracketInnerPipeToken :> ISyntaxNode
                | 1 -> element :> ISyntaxNode
                | 2 -> rightBracketInnerPipeToken :> ISyntaxNode
                | _ -> failwith \"invalid slot\"
    
        member this.SlotCount =
            match this with
            | BracketInnerPipes _ -> 3

        member this.FullWidth =
            match this with
            | BracketInnerPipes(fullWidth=fullWidth) -> fullWidth

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxCurlyBrackets<'T when 'T :> ISyntaxNode> =
    | CurlyBrackets
        of
        leftCurlyBracketToken: SyntaxToken *
        element: 'T *
        rightCurlyBracketToken: SyntaxToken *
        fullWidth: int

    interface ISyntaxNode with
    
        member this.IsTerminal = false
    
        member this.IsToken = false
    
        member this.IsError = false
    
        member this.GetSlot(index) =
            match this with
            | CurlyBrackets(leftCurlyBracketToken, element, rightCurlyBracketToken, _) ->
                match index with
                | 0 -> leftCurlyBracketToken :> ISyntaxNode
                | 1 -> element :> ISyntaxNode
                | 2 -> rightCurlyBracketToken :> ISyntaxNode
                | _ -> failwith \"invalid slot\"
    
        member this.SlotCount =
            match this with
            | CurlyBrackets _ -> 3

        member this.FullWidth =
            match this with
            | CurlyBrackets(fullWidth=fullWidth) -> fullWidth

[<RequireQualifiedAccess;NoComparison;ReferenceEquality>]
type SyntaxToken =
    | Token of token: Token
    | TokenWithTrivia
        of
        leadingTrivia: SyntaxToken *
        token: Token *
        fullWidth: int

    interface ISyntaxNode with

        member this.IsTerminal = false

        member this.IsToken = true

        member this.IsError = false

        member this.GetSlot(index) =
            match this with
            | Token _ ->
                failwith \"invalid slot\"
            | _ when index > 1 || index < 0 ->
                failwith \"invalid slot\"
            | TokenWithTrivia(leadingTrivia, _, _) ->
                leadingTrivia

        member this.SlotCount =
            match this with
            | Token _ ->
                0
            | TokenWithTrivia _ ->
                1

        member this.FullWidth =
            match this with
            | TokenWithTrivia(fullWidth=fullWidth) -> fullWidth
            | Token(token) -> token.Width
"
    |> addInternal cenv

    add cenv "// Generated File Do Not Modify\n"
    add cenv "namespace rec Oly.Compiler.Syntax\n\n"
    add cenv "open Oly.Core\n"
    add cenv "open Oly.Compiler.Text\n"
    add cenv "open Oly.Compiler.Syntax.Internal\n\n"

    "
[<Sealed;NoComparison>]
type OlySyntaxSeparatorList<'T when 'T :> OlySyntaxNode> internal (tree: OlySyntaxTree, start: int, parent: OlySyntaxNode, internalNode: ISyntaxNode) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    let mutable children: OlySyntaxNode imarray = ImArray.empty
    let mutable childrenOfType = ImArray.empty

    override this.TextSpan =
        let offset = this.GetLeadingTriviaWidth()
        OlyTextSpan.Create(start + offset, this.FullTextSpan.Width - offset)

    override _.FullTextSpan = OlyTextSpan.Create(start, internalNode.FullWidth)

    override _.Children =
        if children.IsEmpty && internalNode.SlotCount > 0 then
            children <-
                let mutable p = start
                ImArray.init 
                    internalNode.SlotCount 
                    (fun i -> 
                        let t = Convert.From(tree, p, this, internalNode.GetSlot(i))
                        p <- p + t.FullTextSpan.Width
                        t
                    )
            childrenOfType <- 
                children 
                |> ImArray.choose (fun x -> match x with :? 'T as x -> Some x | _ -> None)
        children

    member this.ChildrenOfType =
        this.Children |> ignore
        childrenOfType

[<Sealed;NoComparison>]
type OlySyntaxList<'T when 'T :> OlySyntaxNode> internal (tree: OlySyntaxTree, start: int, parent: OlySyntaxNode, internalNode: ISyntaxNode) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    let mutable children: OlySyntaxNode imarray = ImArray.empty
    let mutable childrenOfType = ImArray.empty

    override this.TextSpan =
        let offset = this.GetLeadingTriviaWidth()
        OlyTextSpan.Create(start + offset, this.FullTextSpan.Width - offset)

    override _.FullTextSpan = OlyTextSpan.Create(start, internalNode.FullWidth)

    override _.Children =
        if children.IsEmpty && internalNode.SlotCount > 0 then
            children <-
                let mutable p = start
                ImArray.init 
                    internalNode.SlotCount 
                    (fun i -> 
                        let t = Convert.From(tree, p, this, internalNode.GetSlot(i))
                        p <- p + t.FullTextSpan.Width
                        t
                    )
            childrenOfType <- 
                children 
                |> ImArray.map (fun x -> x :?> 'T)
        children

    member this.ChildrenOfType =
        this.Children |> ignore
        childrenOfType

[<Sealed;NoComparison>]
type OlySyntaxBrackets<'T when 'T :> OlySyntaxNode> internal (tree: OlySyntaxTree, start: int, parent: OlySyntaxNode, internalNode: ISyntaxNode) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)
    
    let mutable children: OlySyntaxNode imarray = ImArray.empty
    let mutable element = Unchecked.defaultof<'T>

    override this.TextSpan =
        let offset = this.GetLeadingTriviaWidth()
        OlyTextSpan.Create(start + offset, this.FullTextSpan.Width - offset)

    override _.FullTextSpan = OlyTextSpan.Create(start, internalNode.FullWidth)

    override _.Children =
        if children.IsEmpty && internalNode.SlotCount > 0 then
            children <-
                let mutable p = start
                ImArray.init 
                    internalNode.SlotCount 
                    (fun i -> 
                        let t = Convert.From(tree, p, this, internalNode.GetSlot(i))
                        p <- p + t.FullTextSpan.Width
                        t
                    )
            element <- children[1] :?> 'T
        children

    member _.Element =
        this.Children |> ignore
        element
    
    member internal _.Internal = internalNode

[<Sealed;NoComparison>]
type OlySyntaxBracketInnerPipes<'T when 'T :> OlySyntaxNode> internal (tree: OlySyntaxTree, start: int, parent: OlySyntaxNode, internalNode: ISyntaxNode) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)
    
    let mutable children: OlySyntaxNode imarray = ImArray.empty
    let mutable element = Unchecked.defaultof<'T>
    let mutable textSpan = Unchecked.defaultof<OlyTextSpan>

    override this.TextSpan =
        let offset = this.GetLeadingTriviaWidth()
        OlyTextSpan.Create(start + offset, this.FullTextSpan.Width - offset)

    override _.FullTextSpan = OlyTextSpan.Create(start, internalNode.FullWidth)

    override _.Children =
        if children.IsEmpty && internalNode.SlotCount > 0 then
            children <-
                let mutable p = start
                ImArray.init 
                    internalNode.SlotCount 
                    (fun i -> 
                        let t = Convert.From(tree, p, this, internalNode.GetSlot(i))
                        p <- p + t.FullTextSpan.Width
                        t
                    )
            element <- children[1] :?> 'T
        children

    member _.Element =
        this.Children |> ignore
        element
    
    member internal _.Internal = internalNode

[<Sealed;NoComparison>]
type OlySyntaxCurlyBrackets<'T when 'T :> OlySyntaxNode> internal (tree: OlySyntaxTree, start: int, parent: OlySyntaxNode, internalNode: ISyntaxNode) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)
    
    let mutable children: OlySyntaxNode imarray = ImArray.empty
    let mutable element = Unchecked.defaultof<'T>

    override this.TextSpan =
        let offset = this.GetLeadingTriviaWidth()
        OlyTextSpan.Create(start + offset, this.FullTextSpan.Width - offset)

    override _.FullTextSpan = OlyTextSpan.Create(start, internalNode.FullWidth)

    override _.Children =
        if children.IsEmpty && internalNode.SlotCount > 0 then
            children <-
                let mutable p = start
                ImArray.init 
                    internalNode.SlotCount 
                    (fun i -> 
                        let t = Convert.From(tree, p, this, internalNode.GetSlot(i))
                        p <- p + t.FullTextSpan.Width
                        t
                    )
            element <- children[1] :?> 'T
        children

    member _.Element =
        this.Children |> ignore
        element
    
    member internal _.Internal = internalNode
"
    |> add cenv

    let tree = findTree cenv
    computeTree cenv tree
    computeConversionTree cenv tree

    File.WriteAllText(InternalOutputFilePath, cenv.internalBuilder.ToString())
    File.WriteAllText(PublicOutputFilePath, cenv.publicBuilder.ToString())

[<EntryPoint>]
let main _ =
    generate()
    0