// Generated File Do Not Modify
namespace rec Oly.Compiler.Syntax

open Oly.Core
open Oly.Compiler.Text
open Oly.Compiler.Syntax.Internal


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
[<Sealed;NoComparison>]
type OlySyntaxAccessor internal (tree, start: int, parent, internalNode: SyntaxAccessor) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxAccessor =

    let (|Public|_|) (node: OlySyntaxAccessor) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAccessor.Public _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Private|_|) (node: OlySyntaxAccessor) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAccessor.Private _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Internal|_|) (node: OlySyntaxAccessor) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAccessor.Internal _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Protected|_|) (node: OlySyntaxAccessor) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAccessor.Protected _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxAccessor) : unit option =
        match node.Internal with
        | SyntaxAccessor.None _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxName internal (tree, start: int, parent, internalNode: SyntaxName) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxName =

    let (|Identifier|_|) (node: OlySyntaxName) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxName.Identifier _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Generic|_|) (node: OlySyntaxName) : ( OlySyntaxName * OlySyntaxTypeArguments ) option =
        match node.Internal with
        | SyntaxName.Generic _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Qualified|_|) (node: OlySyntaxName) : ( OlySyntaxName * OlySyntaxToken * OlySyntaxName ) option =
        match node.Internal with
        | SyntaxName.Qualified _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Parenthesis|_|) (node: OlySyntaxName) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxName.Parenthesis _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxBlittable internal (tree, start: int, parent, internalNode: SyntaxBlittable) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxBlittable =

    let (|Blittable|_|) (node: OlySyntaxBlittable) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxBlittable.Blittable _ ->
            Option.Some (node.Children.[0] :?> _)

[<Sealed;NoComparison>]
type OlySyntaxBlittableOptional internal (tree, start: int, parent, internalNode: SyntaxBlittableOptional) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxBlittableOptional =

    let (|Some|_|) (node: OlySyntaxBlittableOptional) : ( OlySyntaxBlittable ) option =
        match node.Internal with
        | SyntaxBlittableOptional.Some _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxBlittableOptional) : unit option =
        match node.Internal with
        | SyntaxBlittableOptional.None _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxAttribute internal (tree, start: int, parent, internalNode: SyntaxAttribute) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxAttribute =

    let (|AutoOpen|_|) (node: OlySyntaxAttribute) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAttribute.AutoOpen _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Nullable|_|) (node: OlySyntaxAttribute) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAttribute.Nullable _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Blittable|_|) (node: OlySyntaxAttribute) : ( OlySyntaxBlittable ) option =
        match node.Internal with
        | SyntaxAttribute.Blittable _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Intrinsic|_|) (node: OlySyntaxAttribute) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAttribute.Intrinsic _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

    let (|Import|_|) (node: OlySyntaxAttribute) : ( OlySyntaxName * OlySyntaxArguments ) option =
        match node.Internal with
        | SyntaxAttribute.Import _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Export|_|) (node: OlySyntaxAttribute) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAttribute.Export _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Inline|_|) (node: OlySyntaxAttribute) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAttribute.Inline _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|NotInline|_|) (node: OlySyntaxAttribute) : ( OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAttribute.NotInline _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Pure|_|) (node: OlySyntaxAttribute) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAttribute.Pure _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Expression|_|) (node: OlySyntaxAttribute) : ( OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxAttribute.Expression _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxAttribute) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxAttribute.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxHashAttribute internal (tree, start: int, parent, internalNode: SyntaxHashAttribute) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxHashAttribute =

    let (|HashAttribute|_|) (node: OlySyntaxHashAttribute) : ( OlySyntaxToken * OlySyntaxAttribute OlySyntaxBrackets ) option =
        match node.Internal with
        | SyntaxHashAttribute.HashAttribute _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)

[<Sealed;NoComparison>]
type OlySyntaxAttributes internal (tree, start: int, parent, internalNode: SyntaxAttributes) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxAttributes =

    let (|Attributes|_|) (node: OlySyntaxAttributes) : ( OlySyntaxHashAttribute OlySyntaxList ) option =
        match node.Internal with
        | SyntaxAttributes.Attributes _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Empty|_|) (node: OlySyntaxAttributes) : unit option =
        match node.Internal with
        | SyntaxAttributes.Empty _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxConstraint internal (tree, start: int, parent, internalNode: SyntaxConstraint) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxConstraint =

    let (|Null|_|) (node: OlySyntaxConstraint) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxConstraint.Null _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Struct|_|) (node: OlySyntaxConstraint) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxConstraint.Struct _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|NotStruct|_|) (node: OlySyntaxConstraint) : ( OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxConstraint.NotStruct _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Unmanaged|_|) (node: OlySyntaxConstraint) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxConstraint.Unmanaged _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Type|_|) (node: OlySyntaxConstraint) : ( OlySyntaxType ) option =
        match node.Internal with
        | SyntaxConstraint.Type _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxConstraint) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxConstraint.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|ConstantType|_|) (node: OlySyntaxConstraint) : ( OlySyntaxToken * OlySyntaxType ) option =
        match node.Internal with
        | SyntaxConstraint.ConstantType _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxConstraintClause internal (tree, start: int, parent, internalNode: SyntaxConstraintClause) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxConstraintClause =

    let (|ConstraintClause|_|) (node: OlySyntaxConstraintClause) : ( OlySyntaxToken * OlySyntaxType * OlySyntaxToken * OlySyntaxConstraint OlySyntaxSeparatorList ) option =
        match node.Internal with
        | SyntaxConstraintClause.ConstraintClause _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxConstraintClause) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxConstraintClause.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxTypeParameters internal (tree, start: int, parent, internalNode: SyntaxTypeParameters) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxTypeParameters =

    let (|TypeParameters|_|) (node: OlySyntaxTypeParameters) : ( OlySyntaxToken * OlySyntaxType OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeParameters.TypeParameters _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|RequireTypeParameters|_|) (node: OlySyntaxTypeParameters) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxType OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeParameters.RequireTypeParameters _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

    let (|Empty|_|) (node: OlySyntaxTypeParameters) : unit option =
        match node.Internal with
        | SyntaxTypeParameters.Empty _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxTypeConstructor internal (tree, start: int, parent, internalNode: SyntaxTypeConstructor) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxTypeConstructor =

    let (|Identifier|_|) (node: OlySyntaxTypeConstructor) : ( OlySyntaxToken * OlySyntaxTypeParameters ) option =
        match node.Internal with
        | SyntaxTypeConstructor.Identifier _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|QualifiedName|_|) (node: OlySyntaxTypeConstructor) : ( OlySyntaxToken * OlySyntaxTypeParameters * OlySyntaxToken * OlySyntaxTypeConstructor ) option =
        match node.Internal with
        | SyntaxTypeConstructor.QualifiedName _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxTupleElement internal (tree, start: int, parent, internalNode: SyntaxTupleElement) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxTupleElement =

    let (|Type|_|) (node: OlySyntaxTupleElement) : ( OlySyntaxType ) option =
        match node.Internal with
        | SyntaxTupleElement.Type _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|IdentifierWithTypeAnnotation|_|) (node: OlySyntaxTupleElement) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxType ) option =
        match node.Internal with
        | SyntaxTupleElement.IdentifierWithTypeAnnotation _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxTupleElement) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTupleElement.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxType internal (tree, start: int, parent, internalNode: SyntaxType) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxType =

    let (|Name|_|) (node: OlySyntaxType) : ( OlySyntaxName ) option =
        match node.Internal with
        | SyntaxType.Name _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Tuple|_|) (node: OlySyntaxType) : ( OlySyntaxToken * OlySyntaxTupleElement OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxType.Tuple _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Variadic|_|) (node: OlySyntaxType) : ( OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxType.Variadic _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|VariadicIndexer|_|) (node: OlySyntaxType) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxType.VariadicIndexer _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _)
        | _ ->
            Option.None

    let (|Array|_|) (node: OlySyntaxType) : ( OlySyntaxType * OlySyntaxToken OlySyntaxList OlySyntaxBrackets ) option =
        match node.Internal with
        | SyntaxType.Array _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|MutableArray|_|) (node: OlySyntaxType) : ( OlySyntaxType * OlySyntaxToken OlySyntaxList OlySyntaxBracketInnerPipes ) option =
        match node.Internal with
        | SyntaxType.MutableArray _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Shape|_|) (node: OlySyntaxType) : ( OlySyntaxExpression OlySyntaxSeparatorList OlySyntaxCurlyBrackets ) option =
        match node.Internal with
        | SyntaxType.Shape _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|WildCard|_|) (node: OlySyntaxType) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxType.WildCard _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Function|_|) (node: OlySyntaxType) : ( OlySyntaxType * OlySyntaxToken * OlySyntaxType ) option =
        match node.Internal with
        | SyntaxType.Function _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|FunctionPtr|_|) (node: OlySyntaxType) : ( OlySyntaxToken * OlySyntaxBlittableOptional * OlySyntaxType * OlySyntaxToken * OlySyntaxType ) option =
        match node.Internal with
        | SyntaxType.FunctionPtr _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _)
        | _ ->
            Option.None

    let (|Postfix|_|) (node: OlySyntaxType) : ( OlySyntaxType * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxType.Postfix _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Literal|_|) (node: OlySyntaxType) : ( OlySyntaxLiteral ) option =
        match node.Internal with
        | SyntaxType.Literal _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxType) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxType.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxMutability internal (tree, start: int, parent, internalNode: SyntaxMutability) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxMutability =

    let (|Mutable|_|) (node: OlySyntaxMutability) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxMutability.Mutable _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxMutability) : unit option =
        match node.Internal with
        | SyntaxMutability.None _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxParameter internal (tree, start: int, parent, internalNode: SyntaxParameter) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxParameter =

    let (|Identifier|_|) (node: OlySyntaxParameter) : ( OlySyntaxAttributes * OlySyntaxMutability * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxParameter.Identifier _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|IdentifierWithTypeAnnotation|_|) (node: OlySyntaxParameter) : ( OlySyntaxAttributes * OlySyntaxMutability * OlySyntaxToken * OlySyntaxToken * OlySyntaxType ) option =
        match node.Internal with
        | SyntaxParameter.IdentifierWithTypeAnnotation _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _)
        | _ ->
            Option.None

    let (|Type|_|) (node: OlySyntaxParameter) : ( OlySyntaxAttributes * OlySyntaxType ) option =
        match node.Internal with
        | SyntaxParameter.Type _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxParameter) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxParameter.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxTypeArguments internal (tree, start: int, parent, internalNode: SyntaxTypeArguments) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxTypeArguments =

    let (|TypeArguments|_|) (node: OlySyntaxTypeArguments) : ( OlySyntaxToken * OlySyntaxType OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeArguments.TypeArguments _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Empty|_|) (node: OlySyntaxTypeArguments) : unit option =
        match node.Internal with
        | SyntaxTypeArguments.Empty _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxParameters internal (tree, start: int, parent, internalNode: SyntaxParameters) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxParameters =

    let (|Parameters|_|) (node: OlySyntaxParameters) : ( OlySyntaxToken * OlySyntaxParameter OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxParameters.Parameters _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Empty|_|) (node: OlySyntaxParameters) : unit option =
        match node.Internal with
        | SyntaxParameters.Empty _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxLambdaKind internal (tree, start: int, parent, internalNode: SyntaxLambdaKind) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxLambdaKind =

    let (|Static|_|) (node: OlySyntaxLambdaKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLambdaKind.Static _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxLambdaKind) : unit option =
        match node.Internal with
        | SyntaxLambdaKind.None _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxReturnTypeAnnotation internal (tree, start: int, parent, internalNode: SyntaxReturnTypeAnnotation) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxReturnTypeAnnotation =

    let (|TypeAnnotation|_|) (node: OlySyntaxReturnTypeAnnotation) : ( OlySyntaxToken * OlySyntaxType ) option =
        match node.Internal with
        | SyntaxReturnTypeAnnotation.TypeAnnotation _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxReturnTypeAnnotation) : unit option =
        match node.Internal with
        | SyntaxReturnTypeAnnotation.None _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxFunctionName internal (tree, start: int, parent, internalNode: SyntaxFunctionName) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxFunctionName =

    let (|Identifier|_|) (node: OlySyntaxFunctionName) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxFunctionName.Identifier _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Parenthesis|_|) (node: OlySyntaxFunctionName) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxFunctionName.Parenthesis _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxBindingDeclaration internal (tree, start: int, parent, internalNode: SyntaxBindingDeclaration) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxBindingDeclaration =

    let (|Function|_|) (node: OlySyntaxBindingDeclaration) : ( OlySyntaxFunctionName * OlySyntaxTypeParameters * OlySyntaxParameters * OlySyntaxReturnTypeAnnotation * OlySyntaxConstraintClause OlySyntaxSeparatorList ) option =
        match node.Internal with
        | SyntaxBindingDeclaration.Function _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _)
        | _ ->
            Option.None

    let (|Value|_|) (node: OlySyntaxBindingDeclaration) : ( OlySyntaxToken * OlySyntaxReturnTypeAnnotation ) option =
        match node.Internal with
        | SyntaxBindingDeclaration.Value _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|New|_|) (node: OlySyntaxBindingDeclaration) : ( OlySyntaxToken * OlySyntaxParameters ) option =
        match node.Internal with
        | SyntaxBindingDeclaration.New _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Get|_|) (node: OlySyntaxBindingDeclaration) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxBindingDeclaration.Get _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Set|_|) (node: OlySyntaxBindingDeclaration) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxBindingDeclaration.Set _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Getter|_|) (node: OlySyntaxBindingDeclaration) : ( OlySyntaxToken * OlySyntaxParameters ) option =
        match node.Internal with
        | SyntaxBindingDeclaration.Getter _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Setter|_|) (node: OlySyntaxBindingDeclaration) : ( OlySyntaxToken * OlySyntaxParameters ) option =
        match node.Internal with
        | SyntaxBindingDeclaration.Setter _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxBindingDeclaration) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxBindingDeclaration.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxPropertyBinding internal (tree, start: int, parent, internalNode: SyntaxPropertyBinding) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxPropertyBinding =

    let (|Binding|_|) (node: OlySyntaxPropertyBinding) : ( OlySyntaxAttributes * OlySyntaxAccessor * OlySyntaxValueDeclarationPremodifier OlySyntaxList * OlySyntaxValueDeclarationKind * OlySyntaxBinding ) option =
        match node.Internal with
        | SyntaxPropertyBinding.Binding _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _)

[<Sealed;NoComparison>]
type OlySyntaxGuardBinding internal (tree, start: int, parent, internalNode: SyntaxGuardBinding) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxGuardBinding =

    let (|Implementation|_|) (node: OlySyntaxGuardBinding) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression * OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxGuardBinding.Implementation _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _, node.Children.[5] :?> _, node.Children.[6] :?> _)
        | _ ->
            Option.None

    let (|Signature|_|) (node: OlySyntaxGuardBinding) : ( OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxGuardBinding.Signature _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxBinding internal (tree, start: int, parent, internalNode: SyntaxBinding) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxBinding =

    let (|Implementation|_|) (node: OlySyntaxBinding) : ( OlySyntaxBindingDeclaration * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxBinding.Implementation _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Signature|_|) (node: OlySyntaxBinding) : ( OlySyntaxBindingDeclaration ) option =
        match node.Internal with
        | SyntaxBinding.Signature _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Property|_|) (node: OlySyntaxBinding) : ( OlySyntaxBindingDeclaration * OlySyntaxPropertyBinding OlySyntaxSeparatorList ) option =
        match node.Internal with
        | SyntaxBinding.Property _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|PropertyWithDefault|_|) (node: OlySyntaxBinding) : ( OlySyntaxBindingDeclaration * OlySyntaxPropertyBinding OlySyntaxSeparatorList * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxBinding.PropertyWithDefault _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

    let (|PatternWithGuard|_|) (node: OlySyntaxBinding) : ( OlySyntaxBindingDeclaration * OlySyntaxGuardBinding ) option =
        match node.Internal with
        | SyntaxBinding.PatternWithGuard _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxTypeDeclarationKind internal (tree, start: int, parent, internalNode: SyntaxTypeDeclarationKind) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxTypeDeclarationKind =

    let (|Alias|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.Alias _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Class|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.Class _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|AbstractClass|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.AbstractClass _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|SealedClass|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.SealedClass _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Interface|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.Interface _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|SealedInterface|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.SealedInterface _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Module|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.Module _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Shape|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.Shape _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Struct|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.Struct _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Extension|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.Extension _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Attribute|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.Attribute _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Enum|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.Enum _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Newtype|_|) (node: OlySyntaxTypeDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationKind.Newtype _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxLiteral internal (tree, start: int, parent, internalNode: SyntaxLiteral) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxLiteral =

    let (|Int8|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Int8 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|UInt8|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.UInt8 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Int16|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Int16 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|UInt16|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.UInt16 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Int32|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Int32 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|UInt32|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.UInt32 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Int64|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Int64 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|UInt64|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.UInt64 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Float32|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Float32 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Float64|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Float64 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Bool|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Bool _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Char16|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Char16 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Utf16|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Utf16 _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Null|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Null _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Default|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Default _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|UncheckedDefault|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.UncheckedDefault _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Integer|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Integer _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Real|_|) (node: OlySyntaxLiteral) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxLiteral.Real _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxFieldPattern internal (tree, start: int, parent, internalNode: SyntaxFieldPattern) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxFieldPattern =

    let (|FieldPattern|_|) (node: OlySyntaxFieldPattern) : ( OlySyntaxName * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxFieldPattern.FieldPattern _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxFieldPattern) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxFieldPattern.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxNamedArgument internal (tree, start: int, parent, internalNode: SyntaxNamedArgument) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxNamedArgument =

    let (|NamedArgument|_|) (node: OlySyntaxNamedArgument) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxNamedArgument.NamedArgument _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)

[<Sealed;NoComparison>]
type OlySyntaxArguments internal (tree, start: int, parent, internalNode: SyntaxArguments) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxArguments =

    let (|Arguments|_|) (node: OlySyntaxArguments) : ( OlySyntaxToken * OlySyntaxExpression OlySyntaxSeparatorList * OlySyntaxNamedArgument OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxArguments.Arguments _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

    let (|Empty|_|) (node: OlySyntaxArguments) : unit option =
        match node.Internal with
        | SyntaxArguments.Empty _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxElseIfOrElseExpression internal (tree, start: int, parent, internalNode: SyntaxElseIfOrElseExpression) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxElseIfOrElseExpression =

    let (|ElseIf|_|) (node: OlySyntaxElseIfOrElseExpression) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression * OlySyntaxToken * OlySyntaxExpression * OlySyntaxElseIfOrElseExpression ) option =
        match node.Internal with
        | SyntaxElseIfOrElseExpression.ElseIf _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _, node.Children.[5] :?> _, node.Children.[6] :?> _)
        | _ ->
            Option.None

    let (|Else|_|) (node: OlySyntaxElseIfOrElseExpression) : ( OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxElseIfOrElseExpression.Else _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxElseIfOrElseExpression) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxElseIfOrElseExpression.None _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxCatchOrFinallyExpression internal (tree, start: int, parent, internalNode: SyntaxCatchOrFinallyExpression) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxCatchOrFinallyExpression =

    let (|Catch|_|) (node: OlySyntaxCatchOrFinallyExpression) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxParameter * OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression * OlySyntaxCatchOrFinallyExpression ) option =
        match node.Internal with
        | SyntaxCatchOrFinallyExpression.Catch _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _, node.Children.[5] :?> _, node.Children.[6] :?> _)
        | _ ->
            Option.None

    let (|Finally|_|) (node: OlySyntaxCatchOrFinallyExpression) : ( OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxCatchOrFinallyExpression.Finally _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxCatchOrFinallyExpression) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxCatchOrFinallyExpression.None _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxValueDeclarationPremodifier internal (tree, start: int, parent, internalNode: SyntaxValueDeclarationPremodifier) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxValueDeclarationPremodifier =

    let (|Static|_|) (node: OlySyntaxValueDeclarationPremodifier) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationPremodifier.Static _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Overrides|_|) (node: OlySyntaxValueDeclarationPremodifier) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationPremodifier.Overrides _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Abstract|_|) (node: OlySyntaxValueDeclarationPremodifier) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationPremodifier.Abstract _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Default|_|) (node: OlySyntaxValueDeclarationPremodifier) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationPremodifier.Default _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Mutable|_|) (node: OlySyntaxValueDeclarationPremodifier) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationPremodifier.Mutable _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|New|_|) (node: OlySyntaxValueDeclarationPremodifier) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationPremodifier.New _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxValueDeclarationPostmodifier internal (tree, start: int, parent, internalNode: SyntaxValueDeclarationPostmodifier) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxValueDeclarationPostmodifier =

    let (|Mutable|_|) (node: OlySyntaxValueDeclarationPostmodifier) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationPostmodifier.Mutable _ ->
            Option.Some (node.Children.[0] :?> _)

[<Sealed;NoComparison>]
type OlySyntaxValueDeclarationKind internal (tree, start: int, parent, internalNode: SyntaxValueDeclarationKind) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxValueDeclarationKind =

    let (|Let|_|) (node: OlySyntaxValueDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationKind.Let _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|LetBind|_|) (node: OlySyntaxValueDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationKind.LetBind _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Constant|_|) (node: OlySyntaxValueDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationKind.Constant _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Pattern|_|) (node: OlySyntaxValueDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationKind.Pattern _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxValueDeclarationKind) : unit option =
        match node.Internal with
        | SyntaxValueDeclarationKind.None _ ->
            Option.Some()
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxValueDeclarationKind) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxValueDeclarationKind.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxExtends internal (tree, start: int, parent, internalNode: SyntaxExtends) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxExtends =

    let (|Type|_|) (node: OlySyntaxExtends) : ( OlySyntaxType ) option =
        match node.Internal with
        | SyntaxExtends.Type _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Inherits|_|) (node: OlySyntaxExtends) : ( OlySyntaxToken * OlySyntaxType OlySyntaxSeparatorList ) option =
        match node.Internal with
        | SyntaxExtends.Inherits _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Empty|_|) (node: OlySyntaxExtends) : unit option =
        match node.Internal with
        | SyntaxExtends.Empty _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxImplements internal (tree, start: int, parent, internalNode: SyntaxImplements) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxImplements =

    let (|Implements|_|) (node: OlySyntaxImplements) : ( OlySyntaxToken * OlySyntaxType OlySyntaxSeparatorList ) option =
        match node.Internal with
        | SyntaxImplements.Implements _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Empty|_|) (node: OlySyntaxImplements) : unit option =
        match node.Internal with
        | SyntaxImplements.Empty _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxTypeDeclarationCase internal (tree, start: int, parent, internalNode: SyntaxTypeDeclarationCase) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxTypeDeclarationCase =

    let (|Case|_|) (node: OlySyntaxTypeDeclarationCase) : ( OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationCase.Case _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|EnumCase|_|) (node: OlySyntaxTypeDeclarationCase) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxTypeDeclarationCase.EnumCase _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxTypeDeclarationBody internal (tree, start: int, parent, internalNode: SyntaxTypeDeclarationBody) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxTypeDeclarationBody =

    let (|Body|_|) (node: OlySyntaxTypeDeclarationBody) : ( OlySyntaxExtends * OlySyntaxImplements * OlySyntaxTypeDeclarationCase OlySyntaxList * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxTypeDeclarationBody.Body _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxTypeDeclarationBody) : unit option =
        match node.Internal with
        | SyntaxTypeDeclarationBody.None _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxTypeDeclarationName internal (tree, start: int, parent, internalNode: SyntaxTypeDeclarationName) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxTypeDeclarationName =

    let (|Identifier|_|) (node: OlySyntaxTypeDeclarationName) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationName.Identifier _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Parenthesis|_|) (node: OlySyntaxTypeDeclarationName) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxTypeDeclarationName.Parenthesis _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxPattern internal (tree, start: int, parent, internalNode: SyntaxPattern) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxPattern =

    let (|Literal|_|) (node: OlySyntaxPattern) : ( OlySyntaxLiteral ) option =
        match node.Internal with
        | SyntaxPattern.Literal _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Name|_|) (node: OlySyntaxPattern) : ( OlySyntaxName ) option =
        match node.Internal with
        | SyntaxPattern.Name _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Function|_|) (node: OlySyntaxPattern) : ( OlySyntaxName * OlySyntaxToken * OlySyntaxPattern OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxPattern.Function _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

    let (|Parenthesis|_|) (node: OlySyntaxPattern) : ( OlySyntaxToken * OlySyntaxPattern OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxPattern.Parenthesis _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Discard|_|) (node: OlySyntaxPattern) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxPattern.Discard _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxPattern) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxPattern.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxMatchGuard internal (tree, start: int, parent, internalNode: SyntaxMatchGuard) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxMatchGuard =

    let (|MatchGuard|_|) (node: OlySyntaxMatchGuard) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxMatchGuard.MatchGuard _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxMatchGuard) : unit option =
        match node.Internal with
        | SyntaxMatchGuard.None _ ->
            Option.Some()
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxMatchPattern internal (tree, start: int, parent, internalNode: SyntaxMatchPattern) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxMatchPattern =

    let (|Patterns|_|) (node: OlySyntaxMatchPattern) : ( OlySyntaxPattern OlySyntaxSeparatorList ) option =
        match node.Internal with
        | SyntaxMatchPattern.Patterns _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Or|_|) (node: OlySyntaxMatchPattern) : ( OlySyntaxMatchPattern * OlySyntaxToken * OlySyntaxMatchPattern ) option =
        match node.Internal with
        | SyntaxMatchPattern.Or _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Discard|_|) (node: OlySyntaxMatchPattern) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxMatchPattern.Discard _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxMatchPattern) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxMatchPattern.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxMatchClause internal (tree, start: int, parent, internalNode: SyntaxMatchClause) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxMatchClause =

    let (|MatchClause|_|) (node: OlySyntaxMatchClause) : ( OlySyntaxToken * OlySyntaxMatchPattern * OlySyntaxMatchGuard * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxMatchClause.MatchClause _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _)

[<Sealed;NoComparison>]
type OlySyntaxConstructType internal (tree, start: int, parent, internalNode: SyntaxConstructType) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxConstructType =

    let (|Anonymous|_|) (node: OlySyntaxConstructType) : ( OlySyntaxToken * OlySyntaxFieldPattern OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxConstructType.Anonymous _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Named|_|) (node: OlySyntaxConstructType) : ( OlySyntaxName * OlySyntaxToken * OlySyntaxFieldPattern OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxConstructType.Named _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxCompilationUnit internal (tree, start: int, parent, internalNode: SyntaxCompilationUnit) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxCompilationUnit =

    let (|Namespace|_|) (node: OlySyntaxCompilationUnit) : ( OlySyntaxToken * OlySyntaxName * OlySyntaxTypeDeclarationBody * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxCompilationUnit.Namespace _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

    let (|Module|_|) (node: OlySyntaxCompilationUnit) : ( OlySyntaxAttributes * OlySyntaxAccessor * OlySyntaxToken * OlySyntaxName * OlySyntaxConstraintClause OlySyntaxSeparatorList * OlySyntaxTypeDeclarationBody * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxCompilationUnit.Module _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _, node.Children.[5] :?> _, node.Children.[6] :?> _)
        | _ ->
            Option.None

    let (|AnonymousModule|_|) (node: OlySyntaxCompilationUnit) : ( OlySyntaxTypeDeclarationBody * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxCompilationUnit.AnonymousModule _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

[<Sealed;NoComparison>]
type OlySyntaxExpression internal (tree, start: int, parent, internalNode: SyntaxExpression) as this =
    inherit OlySyntaxNode(tree, parent, internalNode)

    
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

[<RequireQualifiedAccess>]
module OlySyntaxExpression =

    let (|OpenDeclaration|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxName ) option =
        match node.Internal with
        | SyntaxExpression.OpenDeclaration _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|OpenStaticDeclaration|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxName ) option =
        match node.Internal with
        | SyntaxExpression.OpenStaticDeclaration _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|OpenExtensionDeclaration|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxName ) option =
        match node.Internal with
        | SyntaxExpression.OpenExtensionDeclaration _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Sequential|_|) (node: OlySyntaxExpression) : ( OlySyntaxExpression * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxExpression.Sequential _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Parenthesis|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxExpression OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxExpression.Parenthesis _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Array|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxExpression OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxExpression.Array _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|MutableArray|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxExpression OlySyntaxSeparatorList * OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxExpression.MutableArray _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Call|_|) (node: OlySyntaxExpression) : ( OlySyntaxExpression * OlySyntaxArguments ) option =
        match node.Internal with
        | SyntaxExpression.Call _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|InfixCall|_|) (node: OlySyntaxExpression) : ( OlySyntaxExpression * OlySyntaxName * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxExpression.InfixCall _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|PrefixCall|_|) (node: OlySyntaxExpression) : ( OlySyntaxName * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxExpression.PrefixCall _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Throw|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxExpression.Throw _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Indexer|_|) (node: OlySyntaxExpression) : ( OlySyntaxExpression * OlySyntaxExpression OlySyntaxSeparatorList OlySyntaxBrackets ) option =
        match node.Internal with
        | SyntaxExpression.Indexer _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _)
        | _ ->
            Option.None

    let (|Name|_|) (node: OlySyntaxExpression) : ( OlySyntaxName ) option =
        match node.Internal with
        | SyntaxExpression.Name _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|Literal|_|) (node: OlySyntaxExpression) : ( OlySyntaxLiteral ) option =
        match node.Internal with
        | SyntaxExpression.Literal _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|CreateRecord|_|) (node: OlySyntaxExpression) : ( OlySyntaxConstructType ) option =
        match node.Internal with
        | SyntaxExpression.CreateRecord _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|UpdateRecord|_|) (node: OlySyntaxExpression) : ( OlySyntaxExpression * OlySyntaxToken * OlySyntaxConstructType ) option =
        match node.Internal with
        | SyntaxExpression.UpdateRecord _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|If|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression * OlySyntaxToken * OlySyntaxExpression * OlySyntaxElseIfOrElseExpression ) option =
        match node.Internal with
        | SyntaxExpression.If _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _, node.Children.[5] :?> _)
        | _ ->
            Option.None

    let (|Try|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxExpression * OlySyntaxCatchOrFinallyExpression ) option =
        match node.Internal with
        | SyntaxExpression.Try _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Match|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression OlySyntaxSeparatorList * OlySyntaxToken * OlySyntaxMatchClause OlySyntaxList ) option =
        match node.Internal with
        | SyntaxExpression.Match _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _)
        | _ ->
            Option.None

    let (|While|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken * OlySyntaxToken * OlySyntaxExpression * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxExpression.While _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _)
        | _ ->
            Option.None

    let (|Typed|_|) (node: OlySyntaxExpression) : ( OlySyntaxExpression * OlySyntaxToken * OlySyntaxType ) option =
        match node.Internal with
        | SyntaxExpression.Typed _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|ValueDeclaration|_|) (node: OlySyntaxExpression) : ( OlySyntaxAttributes * OlySyntaxAccessor * OlySyntaxValueDeclarationPremodifier OlySyntaxList * OlySyntaxValueDeclarationKind * OlySyntaxValueDeclarationPostmodifier OlySyntaxList * OlySyntaxBinding ) option =
        match node.Internal with
        | SyntaxExpression.ValueDeclaration _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _, node.Children.[5] :?> _)
        | _ ->
            Option.None

    let (|TypeDeclaration|_|) (node: OlySyntaxExpression) : ( OlySyntaxAttributes * OlySyntaxAccessor * OlySyntaxTypeDeclarationKind * OlySyntaxTypeDeclarationName * OlySyntaxTypeParameters * OlySyntaxConstraintClause OlySyntaxSeparatorList * OlySyntaxToken * OlySyntaxTypeDeclarationBody ) option =
        match node.Internal with
        | SyntaxExpression.TypeDeclaration _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _, node.Children.[4] :?> _, node.Children.[5] :?> _, node.Children.[6] :?> _, node.Children.[7] :?> _)
        | _ ->
            Option.None

    let (|MemberAccess|_|) (node: OlySyntaxExpression) : ( OlySyntaxExpression * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxExpression.MemberAccess _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Mutate|_|) (node: OlySyntaxExpression) : ( OlySyntaxExpression * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxExpression.Mutate _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _)
        | _ ->
            Option.None

    let (|Lambda|_|) (node: OlySyntaxExpression) : ( OlySyntaxLambdaKind * OlySyntaxParameters * OlySyntaxToken * OlySyntaxExpression ) option =
        match node.Internal with
        | SyntaxExpression.Lambda _ ->
            Option.Some (node.Children.[0] :?> _, node.Children.[1] :?> _, node.Children.[2] :?> _, node.Children.[3] :?> _)
        | _ ->
            Option.None

    let (|Error|_|) (node: OlySyntaxExpression) : ( OlySyntaxToken ) option =
        match node.Internal with
        | SyntaxExpression.Error _ ->
            Option.Some (node.Children.[0] :?> _)
        | _ ->
            Option.None

    let (|None|_|) (node: OlySyntaxExpression) : unit option =
        match node.Internal with
        | SyntaxExpression.None _ ->
            Option.Some()
        | _ ->
            Option.None

[<RequireQualifiedAccess>]
module private Convert =
    let From(tree: OlySyntaxTree, start: int, parent: OlySyntaxNode, internalNode: ISyntaxNode) : OlySyntaxNode =
        match internalNode.Tag with
        | SyntaxAccessor.Tag -> OlySyntaxAccessor(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxName.Tag -> OlySyntaxName(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxBlittable.Tag -> OlySyntaxBlittable(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxBlittableOptional.Tag -> OlySyntaxBlittableOptional(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxAttribute.Tag -> OlySyntaxAttribute(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxHashAttribute.Tag -> OlySyntaxHashAttribute(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxAttributes.Tag -> OlySyntaxAttributes(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxConstraint.Tag -> OlySyntaxConstraint(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxConstraintClause.Tag -> OlySyntaxConstraintClause(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxTypeParameters.Tag -> OlySyntaxTypeParameters(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxTypeConstructor.Tag -> OlySyntaxTypeConstructor(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxTupleElement.Tag -> OlySyntaxTupleElement(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxType.Tag -> OlySyntaxType(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxMutability.Tag -> OlySyntaxMutability(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxParameter.Tag -> OlySyntaxParameter(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxTypeArguments.Tag -> OlySyntaxTypeArguments(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxParameters.Tag -> OlySyntaxParameters(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxLambdaKind.Tag -> OlySyntaxLambdaKind(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxReturnTypeAnnotation.Tag -> OlySyntaxReturnTypeAnnotation(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxFunctionName.Tag -> OlySyntaxFunctionName(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxBindingDeclaration.Tag -> OlySyntaxBindingDeclaration(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxPropertyBinding.Tag -> OlySyntaxPropertyBinding(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxGuardBinding.Tag -> OlySyntaxGuardBinding(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxBinding.Tag -> OlySyntaxBinding(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxTypeDeclarationKind.Tag -> OlySyntaxTypeDeclarationKind(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxLiteral.Tag -> OlySyntaxLiteral(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxFieldPattern.Tag -> OlySyntaxFieldPattern(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxNamedArgument.Tag -> OlySyntaxNamedArgument(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxArguments.Tag -> OlySyntaxArguments(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxElseIfOrElseExpression.Tag -> OlySyntaxElseIfOrElseExpression(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxCatchOrFinallyExpression.Tag -> OlySyntaxCatchOrFinallyExpression(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxValueDeclarationPremodifier.Tag -> OlySyntaxValueDeclarationPremodifier(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxValueDeclarationPostmodifier.Tag -> OlySyntaxValueDeclarationPostmodifier(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxValueDeclarationKind.Tag -> OlySyntaxValueDeclarationKind(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxExtends.Tag -> OlySyntaxExtends(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxImplements.Tag -> OlySyntaxImplements(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxTypeDeclarationCase.Tag -> OlySyntaxTypeDeclarationCase(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxTypeDeclarationBody.Tag -> OlySyntaxTypeDeclarationBody(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxTypeDeclarationName.Tag -> OlySyntaxTypeDeclarationName(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxPattern.Tag -> OlySyntaxPattern(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxMatchGuard.Tag -> OlySyntaxMatchGuard(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxMatchPattern.Tag -> OlySyntaxMatchPattern(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxMatchClause.Tag -> OlySyntaxMatchClause(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxConstructType.Tag -> OlySyntaxConstructType(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxCompilationUnit.Tag -> OlySyntaxCompilationUnit(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | SyntaxExpression.Tag -> OlySyntaxExpression(tree, start, parent, System.Runtime.CompilerServices.Unsafe.As internalNode) :> OlySyntaxNode
        | Tags.Terminal -> tree.DummyNode
        | _ ->

        match internalNode with
        | :? SyntaxToken as internalNode -> OlySyntaxToken(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxBrackets<SyntaxSeparatorList<SyntaxType>> as internalNode -> OlySyntaxBrackets<OlySyntaxSeparatorList<OlySyntaxType>>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxBracketInnerPipes<SyntaxSeparatorList<SyntaxType>> as internalNode -> OlySyntaxBracketInnerPipes<OlySyntaxSeparatorList<OlySyntaxType>>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxBracketInnerPipes<SyntaxSeparatorList<SyntaxExpression>> as internalNode -> OlySyntaxBracketInnerPipes<OlySyntaxSeparatorList<OlySyntaxExpression>>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxCurlyBrackets<SyntaxSeparatorList<SyntaxType>> as internalNode -> OlySyntaxCurlyBrackets<OlySyntaxSeparatorList<OlySyntaxType>>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxCurlyBrackets<SyntaxSeparatorList<SyntaxExpression>> as internalNode -> OlySyntaxCurlyBrackets<OlySyntaxSeparatorList<OlySyntaxExpression>>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxConstraintClause> as internalNode -> OlySyntaxSeparatorList<OlySyntaxConstraintClause>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxConstraintClause> as internalNode -> OlySyntaxList<OlySyntaxConstraintClause>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxParameter> as internalNode -> OlySyntaxSeparatorList<OlySyntaxParameter>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxTupleElement> as internalNode -> OlySyntaxSeparatorList<OlySyntaxTupleElement>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxParameter> as internalNode -> OlySyntaxList<OlySyntaxParameter>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxExpression> as internalNode -> OlySyntaxSeparatorList<OlySyntaxExpression>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxExpression> as internalNode -> OlySyntaxList<OlySyntaxExpression>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxAttribute> as internalNode -> OlySyntaxSeparatorList<OlySyntaxAttribute>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxAttribute> as internalNode -> OlySyntaxList<OlySyntaxAttribute>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxFieldPattern> as internalNode -> OlySyntaxSeparatorList<OlySyntaxFieldPattern>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxFieldPattern> as internalNode -> OlySyntaxList<OlySyntaxFieldPattern>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxConstraint> as internalNode -> OlySyntaxSeparatorList<OlySyntaxConstraint>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxConstraint> as internalNode -> OlySyntaxList<OlySyntaxConstraint>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxType> as internalNode -> OlySyntaxSeparatorList<OlySyntaxType>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxType> as internalNode -> OlySyntaxList<OlySyntaxType>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxMatchClause> as internalNode -> OlySyntaxSeparatorList<OlySyntaxMatchClause>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxMatchClause> as internalNode -> OlySyntaxList<OlySyntaxMatchClause>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxMatchPattern> as internalNode -> OlySyntaxSeparatorList<OlySyntaxMatchPattern>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxMatchPattern> as internalNode -> OlySyntaxList<OlySyntaxMatchPattern>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxBinding> as internalNode -> OlySyntaxSeparatorList<OlySyntaxBinding>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxBinding> as internalNode -> OlySyntaxList<OlySyntaxBinding>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxPropertyBinding> as internalNode -> OlySyntaxSeparatorList<OlySyntaxPropertyBinding>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxPattern> as internalNode -> OlySyntaxSeparatorList<OlySyntaxPattern>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxPattern> as internalNode -> OlySyntaxList<OlySyntaxPattern>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxBrackets<SyntaxAttribute> as internalNode -> OlySyntaxBrackets<OlySyntaxAttribute>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxHashAttribute> as internalNode -> OlySyntaxList<OlySyntaxHashAttribute>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxSeparatorList<SyntaxNamedArgument> as internalNode -> OlySyntaxSeparatorList<OlySyntaxNamedArgument>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxBrackets<SyntaxList<SyntaxToken>> as internalNode -> OlySyntaxBrackets<OlySyntaxList<OlySyntaxToken>>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxBracketInnerPipes<SyntaxList<SyntaxToken>> as internalNode -> OlySyntaxBracketInnerPipes<OlySyntaxList<OlySyntaxToken>>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxToken> as internalNode -> OlySyntaxList<OlySyntaxToken>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxBrackets<SyntaxSeparatorList<SyntaxExpression>> as internalNode -> OlySyntaxBrackets<OlySyntaxSeparatorList<OlySyntaxExpression>>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxTypeDeclarationCase> as internalNode -> OlySyntaxList<OlySyntaxTypeDeclarationCase>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxValueDeclarationPremodifier> as internalNode -> OlySyntaxList<OlySyntaxValueDeclarationPremodifier>(tree, start, parent, internalNode) :> OlySyntaxNode
        | :? SyntaxList<SyntaxValueDeclarationPostmodifier> as internalNode -> OlySyntaxList<OlySyntaxValueDeclarationPostmodifier>(tree, start, parent, internalNode) :> OlySyntaxNode
        | _ -> failwith "Invalid Internal Syntax Node"
