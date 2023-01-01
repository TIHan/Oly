module LargeSyntaxExample

    let Text =
        let src =
            """
namespace Oly.Memory

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)

interface IMemory<T> where T: struct =

    get_Item(index: int32): byref<T>

    get Length: int32

interface IMemoryAllocator<TMemory<_>> 
    where TMemory<_>: IMemory
    where TMemory: struct
    =

    static abstract Allocate<T>(size: int32): TMemory<T> where T: struct

    static abstract Free<T>(memory: TMemory<T>): () where T: struct

    static abstract Copy<T>(src: TMemory<T>, srcIndex: int32, dest: TMemory<T>, destIndex: int32, length: int32): () where T: struct

struct DefaultMemory<T> where T: struct =
    implements IMemory<T>

    Buffer: T[||]

    private new(buffer: T[||]) =
        {
            Buffer = buffer
        }

    #[inline]
    get_Item(index: int32): byref<T> = &this.Buffer[index]

    #[inline]
    Length: int32
        get() = getLength(this.Buffer)

struct DefaultMemoryAllocator =
    implements IMemoryAllocator<DefaultMemory>

    static overrides Allocate<T>(size: int32): DefaultMemory<T> where T: struct =
        DefaultMemory(Array.ZeroCreate<T>(size))

    static overrides Free<T>(memory: DefaultMemory<T>): () where T: struct =
        ()

    static overrides Copy<T>(src: DefaultMemory<T>, srcIndex: int32, dest: DefaultMemory<T>, destIndex: int32, length: int32): () where T: struct =
        System.Array.Copy(unsafeCast<System.Array>(src.Buffer), srcIndex, unsafeCast<System.Array>(dest.Buffer), destIndex, length)

struct MutableUnorderedList<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable buffer: TMemory<T>
    private mutable count: int32

    new(capacity: int32) =
        {
            buffer = TMemoryAllocator.Allocate(capacity)
            count = 0
        }

    #[inline]
    mutable Add(item: T): () =
        if (this.count + 1 >= this.buffer.Length)
            let prevMemory = this.buffer
            this.buffer <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, 0, this.buffer, 0, prevMemory.Length)
            TMemoryAllocator.Free(prevMemory)

        (this.buffer[this.count]) <- item
        this.count <- this.count + 1

    #[inline]
    mutable GetItem(index: int32): byref<T> =
        &this.buffer[index]

    #[inline]
    GetItem(index: int32): T =
        this.buffer[index]

    #[inline]
    mutable RemoveAt(index: int32): bool =
        (this.buffer[index]) <- this.buffer[this.count - 1]
        this.count <- this.count - 1

    Count: int32 get() = this.count

struct MutableQueue<TMemory<_>, TMemoryAllocator, T>
    where TMemory<_>: IMemory
    where TMemory: struct
    where TMemoryAllocator: IMemoryAllocator<TMemory>, struct
    where T: struct
    =

    private mutable Memory: TMemory<T>
    private mutable Count: int32
    private mutable Cursor: int32

    new(capacity: int32) =
        {
            Memory = TMemoryAllocator.Allocate(capacity)
            Count = 0
            Cursor = 0
        }

    mutable Enqueue(item: T): () =
        if (this.Count + 1 >= this.Memory.Length)
            let prevMemory = this.Memory
            this.Memory <- TMemoryAllocator.Allocate(prevMemory.Length * 2)
            TMemoryAllocator.Copy(prevMemory, this.Cursor, this.Memory, 0, prevMemory.Length - this.Cursor)
            TMemoryAllocator.Copy(prevMemory, 0, this.Memory, prevMemory.Length - this.Cursor, this.Cursor)
            TMemoryAllocator.Free(prevMemory)
            this.Cursor <- 0

        let i =
            if (this.Cursor + this.Count >= this.Memory.Length)
                if (this.Count > this.Memory.Length - this.Cursor)
                    this.Count - (this.Memory.Length - this.Cursor)
                else
                    this.Memory.Length - this.Cursor - this.Count
            else
                this.Cursor + this.Count

        (this.Memory[i]) <- item
        this.Count <- this.Count + 1

    mutable TryDequeue(item: byref<T>): bool =
        if (this.Count == 0)
            false
        else
            item <- this.Memory[this.Cursor]
            this.Count <- this.Count - 1
            this.Cursor <- this.Cursor + 1
            if (this.Cursor >= this.Memory.Length)
                this.Cursor <- 0
            true

    Free(): () =
        TMemoryAllocator.Free(this.Memory)
            """
        Oly.Compiler.Text.OlySourceText.Create(src)
        

