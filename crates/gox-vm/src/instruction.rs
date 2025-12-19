//! Instruction format and opcodes.

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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    Call = 80,    // call func at a, args start at b, c=arg_count, flags=ret_count
    Return,       // return values starting at a, b=count
    
    // ============ Object operations ============
    Alloc = 85,   // a = new object of type b, c=extra_slots
    GetField,     // a = b.field[c]
    SetField,     // a.field[b] = c
    GetFieldN,    // copy c slots from b.field[flags] to a
    SetFieldN,    // copy c slots from b to a.field[flags]
    StructHash,   // a = hash(b) with c fields (for struct map keys)
    
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
    ClosureCall,      // call closure at a, args at b, c=arg_count, flags=ret_count
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
    InitInterface = 175,  // init interface at a with iface_type=b (slot0 high 32 bits)
    BoxInterface,         // a[slot0] = (a[slot0] & 0xFFFF_FFFF_0000_0000) | b, a[slot1] = c
    UnboxInterface,       // a = unbox(b), type at c
    TypeAssert,           // a = b.(type c), ok at flags
    
    // ============ Type conversion ============
    I64ToF64 = 180,
    F64ToI64,
    I32ToI64,
    I64ToI32,
    
    // ============ Extern call ============
    CallExtern = 185, // call extern func a, args at b, c=arg_count, flags=ret_count
    
    // ============ Debug ============
    DebugPrint = 190,
    AssertBegin,      // a=cond, b=arg_count, c=line; if cond==false, begin assert output
    AssertArg,        // a=value, b=type_tag; print arg if in assert failure mode
    AssertEnd,        // end assert; if failed, terminate program
    
    // Invalid
    Invalid = 255,
}

impl Opcode {
    pub fn from_u8(v: u8) -> Self {
        match v {
            0 => Self::Nop,
            1 => Self::LoadNil,
            2 => Self::LoadTrue,
            3 => Self::LoadFalse,
            4 => Self::LoadInt,
            5 => Self::LoadConst,
            6 => Self::Mov,
            7 => Self::MovN,
            
            10 => Self::GetGlobal,
            11 => Self::SetGlobal,
            
            15 => Self::AddI64,
            16 => Self::SubI64,
            17 => Self::MulI64,
            18 => Self::DivI64,
            19 => Self::ModI64,
            20 => Self::NegI64,
            
            25 => Self::AddF64,
            26 => Self::SubF64,
            27 => Self::MulF64,
            28 => Self::DivF64,
            29 => Self::NegF64,
            
            35 => Self::EqI64,
            36 => Self::NeI64,
            37 => Self::LtI64,
            38 => Self::LeI64,
            39 => Self::GtI64,
            40 => Self::GeI64,
            
            45 => Self::EqF64,
            46 => Self::NeF64,
            47 => Self::LtF64,
            48 => Self::LeF64,
            49 => Self::GtF64,
            50 => Self::GeF64,
            
            55 => Self::EqRef,
            56 => Self::NeRef,
            57 => Self::IsNil,
            
            60 => Self::Band,
            61 => Self::Bor,
            62 => Self::Bxor,
            63 => Self::Bnot,
            64 => Self::Shl,
            65 => Self::Shr,
            66 => Self::Ushr,
            
            70 => Self::Not,
            
            75 => Self::Jump,
            76 => Self::JumpIf,
            77 => Self::JumpIfNot,
            
            80 => Self::Call,
            81 => Self::Return,
            
            85 => Self::Alloc,
            86 => Self::GetField,
            87 => Self::SetField,
            88 => Self::GetFieldN,
            89 => Self::SetFieldN,
            90 => Self::StructHash,
            
            95 => Self::ArrayNew,
            96 => Self::ArrayGet,
            97 => Self::ArraySet,
            98 => Self::ArrayLen,
            99 => Self::SliceNew,
            100 => Self::SliceGet,
            101 => Self::SliceSet,
            102 => Self::SliceLen,
            103 => Self::SliceCap,
            104 => Self::SliceSlice,
            105 => Self::SliceAppend,
            
            110 => Self::StrNew,
            111 => Self::StrConcat,
            112 => Self::StrLen,
            113 => Self::StrIndex,
            114 => Self::StrEq,
            115 => Self::StrNe,
            116 => Self::StrLt,
            117 => Self::StrLe,
            118 => Self::StrGt,
            119 => Self::StrGe,
            120 => Self::StrSlice,
            
            125 => Self::MapNew,
            126 => Self::MapGet,
            127 => Self::MapSet,
            128 => Self::MapDelete,
            129 => Self::MapLen,
            
            135 => Self::ChanNew,
            136 => Self::ChanSend,
            137 => Self::ChanRecv,
            138 => Self::ChanClose,
            
            140 => Self::SelectStart,
            141 => Self::SelectSend,
            142 => Self::SelectRecv,
            143 => Self::SelectEnd,
            
            145 => Self::IterBegin,
            146 => Self::IterNext,
            147 => Self::IterEnd,
            
            150 => Self::ClosureNew,
            151 => Self::ClosureGet,
            152 => Self::ClosureSet,
            153 => Self::ClosureCall,
            154 => Self::UpvalNew,
            155 => Self::UpvalGet,
            156 => Self::UpvalSet,
            
            160 => Self::Go,
            161 => Self::Yield,
            
            165 => Self::DeferPush,
            166 => Self::DeferPop,
            167 => Self::ErrDeferPush,
            168 => Self::Panic,
            169 => Self::Recover,
            
            175 => Self::InitInterface,
            176 => Self::BoxInterface,
            177 => Self::UnboxInterface,
            178 => Self::TypeAssert,
            
            180 => Self::I64ToF64,
            181 => Self::F64ToI64,
            182 => Self::I32ToI64,
            183 => Self::I64ToI32,
            
            185 => Self::CallExtern,
            
            190 => Self::DebugPrint,
            191 => Self::AssertBegin,
            192 => Self::AssertArg,
            193 => Self::AssertEnd,
            
            _ => Self::Invalid,
        }
    }
}
