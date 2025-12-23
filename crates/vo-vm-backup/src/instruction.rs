//! Instruction format and opcodes.

use num_enum::TryFromPrimitive;

/// 8-byte instruction format.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Instruction {
    pub op: u8,
    pub flags: u8,
    pub a: u16,
    pub b: u16,
    pub c: u16,
}

impl Instruction {
    pub const fn new(op: Opcode, a: u16, b: u16, c: u16) -> Self {
        Self { op: op as u8, flags: 0, a, b, c }
    }
    
    pub const fn with_flags(op: Opcode, flags: u8, a: u16, b: u16, c: u16) -> Self {
        Self { op: op as u8, flags, a, b, c }
    }
    
    pub fn opcode(&self) -> Opcode {
        Opcode::from_u8(self.op)
    }
    
    /// Get a 32-bit immediate from b and c fields.
    pub fn imm32(&self) -> i32 {
        ((self.b as u32) | ((self.c as u32) << 16)) as i32
    }
    
    /// Get a 32-bit unsigned immediate from b and c fields.
    pub fn uimm32(&self) -> u32 {
        (self.b as u32) | ((self.c as u32) << 16)
    }
}

/// Opcodes.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFromPrimitive)]
pub enum Opcode {
    // ============ Load/Store ============
    Nop = 0,
    LoadNil,      // a = nil
    LoadTrue,     // a = true
    LoadFalse,    // a = false
    LoadInt,      // a = sign_extend(b|c)
    LoadConst,    // a = constants[b]
    Mov,          // a = b
    MovN,         // copy c slots from b to a
    
    // ============ Globals ============
    GetGlobal = 10, // a = globals[b]
    SetGlobal,      // globals[a] = b
    
    // ============ Arithmetic (i64) ============
    AddI64 = 15,  // a = b + c
    SubI64,       // a = b - c
    MulI64,       // a = b * c
    DivI64,       // a = b / c
    ModI64,       // a = b % c
    NegI64,       // a = -b
    
    // ============ Arithmetic (f64) ============
    AddF64 = 25,
    SubF64,
    MulF64,
    DivF64,
    NegF64,
    
    // ============ Comparison (i64) ============
    EqI64 = 35,   // a = (b == c)
    NeI64,        // a = (b != c)
    LtI64,        // a = (b < c)
    LeI64,        // a = (b <= c)
    GtI64,        // a = (b > c)
    GeI64,        // a = (b >= c)
    
    // ============ Comparison (f64) ============
    EqF64 = 45,
    NeF64,
    LtF64,
    LeF64,
    GtF64,
    GeF64,
    
    // ============ Reference comparison ============
    EqRef = 55,   // a = (b == c) for references
    NeRef,        // a = (b != c) for references
    IsNil,        // a = (b == nil)
    
    // ============ Bitwise ============
    Band = 60,    // a = b & c
    Bor,          // a = b | c
    Bxor,         // a = b ^ c
    Bnot,         // a = ^b
    Shl,          // a = b << c
    Shr,          // a = b >> c (arithmetic)
    Ushr,         // a = b >>> c (logical)
    
    // ============ Logical ============
    Not = 70,     // a = !b
    
    // ============ Control flow ============
    Jump = 75,    // pc += imm32(b,c)
    JumpIf,       // if a: pc += imm32(b,c)
    JumpIfNot,    // if !a: pc += imm32(b,c)
    
    // ============ Function call ============
    Call = 80,        // call func[a], args at b, c=arg_count, flags=ret_count
    CallExtern,       // call extern[a], args at b, c=arg_count, flags=ret_count
    CallClosure,      // call closure a, args at b, c=arg_count, flags=ret_count
    CallInterface,    // call iface a method b, args at c, flags=arg_count|ret_count
    Return,           // return values starting at a, b=count
    
    // ============ Object operations ============
    Alloc = 85,   // a = new object, type_id=b|(c<<16), flags=field_count
    GetField,     // a = b.field[c]
    SetField,     // a.field[b] = c
    GetFieldN,    // copy c slots from b.field[flags] to a
    SetFieldN,    // copy c slots from b to a.field[flags]
    StructHash,   // a = hash(b) with c fields (for struct map keys)
    StructClone,  // a = deep copy of struct at b
    
    // ============ Array/Slice ============
    ArrayNew = 95,    // a = new array, elem_type=b, len=c
    ArrayGet,         // a = b[c]
    ArraySet,         // a[b] = c
    ArrayLen,         // a = len(b)
    SliceNew,         // a = new slice, array=b, start=c, flags has end info
    SliceGet,         // a = b[c]
    SliceSet,         // a[b] = c
    SliceLen,         // a = len(b)
    SliceCap,         // a = cap(b)
    SliceSlice,       // a = b[c:flags] (flags encodes end)
    SliceAppend,      // a = append(b, c...)
    
    // ============ String ============
    StrNew = 110,     // a = new string from const b
    StrConcat,        // a = b + c (string concat)
    StrLen,           // a = len(b)
    StrIndex,         // a = b[c]
    StrEq,            // a = (b == c) for strings (content comparison)
    StrNe,            // a = (b != c) for strings (content comparison)
    StrLt,            // a = (b < c) for strings (lexicographic)
    StrLe,            // a = (b <= c) for strings (lexicographic)
    StrGt,            // a = (b > c) for strings (lexicographic)
    StrGe,            // a = (b >= c) for strings (lexicographic)
    StrSlice,         // a = b[c:flags] (string slicing)
    
    // ============ Map ============
    MapNew = 125,     // a = make(map), key_type=b, val_type=c
    MapGet,           // a = map[b], ok stored at c
    MapSet,           // a[b] = c
    MapDelete,        // delete(a, b)
    MapLen,           // a = len(b)
    
    // ============ Channel ============
    ChanNew = 135,    // a = make(chan), elem_type=b, cap=c
    ChanSend,         // a <- b
    ChanRecv,         // a = <-b, ok at c
    ChanClose,        // close(a)
    
    // ============ Select ============
    SelectStart = 140,  // a=case_count, b=has_default; start building select
    SelectSend,         // a=chan, b=value; add send case
    SelectRecv,         // a=dest, b=chan, c=ok_dest; add recv case
    SelectEnd,          // a=dest (chosen case index); execute select and jump
    
    // ============ Iterator (range) ============
    IterBegin = 145,  // begin iteration over a, type=b
    IterNext,         // a,b = next from iter, c=done_offset
    IterEnd,          // end iteration
    
    // ============ Closure ============
    ClosureNew = 150, // a = closure(func=b), c=upvalue_count
    ClosureGet,       // a = closure.upvalues[b]
    ClosureSet,       // closure.upvalues[a] = b
    UpvalNew,         // a = new upval_box (for reference capture)
    UpvalGet,         // a = upval_box[b].value
    UpvalSet,         // upval_box[a].value = b
    
    // ============ Goroutine ============
    Go = 160,         // go call func at a, args at b, c=arg_count
    Yield,            // yield current fiber
    
    // ============ Defer/Panic/Recover ============
    DeferPush = 165,  // defer func at a, args at b, c=arg_count
    DeferPop,         // pop and execute defers for current frame
    ErrDeferPush,     // errdefer func at a, args at b, c=arg_count (only runs on error)
    Panic,            // panic with value at a
    Recover,          // a = recover()
    
    // ============ Interface ============
    InitInterface = 175,  // init interface at a, iface_type=b|(c<<16)
    BoxInterface,         // a[slot0] = (a[slot0] & 0xFFFF_FFFF_0000_0000) | b, a[slot1] = c
    UnboxInterface,       // a = unbox(b), type at c
    TypeAssert,           // a = b.(type c), ok at flags
    
    // ============ Type conversion ============
    I64ToF64 = 180,
    F64ToI64,
    I32ToI64,
    I64ToI32,
    
    // ============ Debug ============
    DebugPrint = 190,
    AssertBegin,      // a=cond, b=arg_count, c=line; if cond==false, begin assert output
    AssertArg,        // a=value, b=type_tag; print arg if in assert failure mode
    AssertEnd,        // end assert; if failed, terminate program
    
    // Invalid
    Invalid = 255,
}

impl Opcode {
    #[inline]
    pub fn from_u8(v: u8) -> Self {
        Self::try_from(v).unwrap_or(Self::Invalid)
    }
}
