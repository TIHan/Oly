namespace rec Oly.Compiler.Syntax.Internal

type internal ISyntaxNode =

    /// Indicates the syntax node is the last node of its siblings.
    abstract IsTerminal : bool

    /// Indicates the syntax node represents a single token.
    abstract IsToken : bool

    /// Indicates the syntax node is an error.
    abstract IsError : bool

    /// Gets a slot (or child) syntax node.
    abstract GetSlot : index: int -> ISyntaxNode

    abstract SlotCount : int

    abstract FullWidth: int