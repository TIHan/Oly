module StressTests

open Xunit
open TestUtilities
open Utilities

[<Fact(Skip = "does not work - stackoverflows compiler")>]
let ``Component Bit Mask stress test``() =
    let src =
        """
#[intrinsic("uint64")]
struct uint64

#[intrinsic("bool")]
struct bool

#[intrinsic("int32")]
struct int32

#[intrinsic("print")]
print(__oly_object): ()

#[intrinsic("by_ref_read_write")]
alias byref<T>

#[intrinsic("by_ref_read")]
alias inref<T>

#[intrinsic("address_of")]
(&)<T>(T): byref<T>

#[intrinsic("address_of")]
(&)<T>(T): inref<T>

#[intrinsic("get_element")]
(`[]`)<T>(T[], index: int32): T
#[intrinsic("get_element")]
(`[,]`)<T>(T[,], index1: int32, index2: int32): T

#[intrinsic("get_element")]
(`[]`)<T>(T[||], index: int32): T
#[intrinsic("set_element")]
(`[]`)<T>(T[||], index: int32, T): ()
#[intrinsic("get_element")]
(`[,]`)<T>(T[|,|], index1: int32, index2: int32): T
#[intrinsic("set_element")]
(`[,]`)<T>(T[|,|], index1: int32, index2: int32, T): T

#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey): TValue where T: { mutable get_Item(TKey): TValue } = x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(x: inref<T>, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(x: T, key: TKey): TValue where T: { get_Item(TKey): TValue } = x.get_Item(key)

#[inline]
(`[]`)<T, TKey, TValue>(x: byref<T>, key: TKey, value: TValue): () where T: { mutable set_Item(TKey, TValue): () } = x.set_Item(key, value)

#[inline]
(`[]`)<T, TKey, TValue>(x: T, key: TKey, value: TValue): () where T: { set_Item(TKey, TValue): () } = x.set_Item(key, value)

#[intrinsic("divide")]
(/)(int32, int32): int32

#[intrinsic("remainder")]
(%)(int32, int32): int32

#[intrinsic("bitwise_shift_left")]
(<<)(uint64, int32): uint64

#[intrinsic("bitwise_not")]
(~)(uint64): uint64

#[intrinsic("bitwise_and")]
(&)(uint64, uint64): uint64

#[intrinsic("bitwise_or")]
(|)(uint64, uint64): uint64

#[intrinsic("not_equal")]
(!=)(uint64, uint64): bool

struct ComponentBitMask =
    private mutable Page0: uint64 = 0
    private mutable Page1: uint64 = 0
    private mutable Page2: uint64 = 0
    private mutable Page3: uint64 = 0
    private mutable Page4: uint64 = 0
    private mutable Page5: uint64 = 0
    private mutable Page6: uint64 = 0
    private mutable Page7: uint64 = 0

    #[inline]
    mutable get_Item(index: int32): byref<uint64> =
        match (index)
        | 0 => &this.Page0
        | 1 => &this.Page1
        | 2 => &this.Page2
        | 3 => &this.Page3
        | 4 => &this.Page4
        | 5 => &this.Page5
        | 6 => &this.Page6
        | _ => &this.Page7

    #[inline]
    get_Item(index: int32): uint64 =
        match (index)
        | 0 => this.Page0
        | 1 => this.Page1
        | 2 => this.Page2
        | 3 => this.Page3
        | 4 => this.Page4
        | 5 => this.Page5
        | 6 => this.Page6
        | _ => this.Page7

    #[inline]
    mutable Set(index: int32, value: bool): () =
        let fieldIndex = index / 64
        let bitIndex = index % 64
        let item = &this.get_Item(fieldIndex)
        if (value)
            item <- item | (1u64 << bitIndex)
        else
            item <- item & ~(1u64 << bitIndex)

    #[inline]
    IsSet(index: int32): bool =
        let fieldIndex = index / 64
        let bitIndex = index % 64
        (this.get_Item(fieldIndex) & (1u64 << bitIndex)) != 0

main(): () =
    let mutable bits = ComponentBitMask()
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
    print(bits.IsSet(0)) // false
    bits.Set(0, true)
    print(bits.IsSet(0)) // true
    bits.Set(0, false)
    print(bits.IsSet(0)) // false
    print(bits.IsSet(500)) // false
    bits.Set(500, true)
    print(bits.IsSet(500)) // true
        """
    Oly src
    |> withCompile
    |> shouldRunWithExpectedOutput "FalseTrueFalseFalseTrue"
    |> ignore