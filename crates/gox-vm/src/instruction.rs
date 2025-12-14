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
    AddI64 = 20,  // a = b + c
    SubI64,       // a = b - c
    MulI64,       // a = b * c
    DivI64,       // a = b / c
    ModI64,       // a = b % c
    NegI64,       // a = -b
    
    // ============ Arithmetic (f64) ============
    AddF64 = 30,
    SubF64,
    MulF64,
    DivF64,
    NegF64,
    
    // ============ Comparison (i64) ============
    EqI64 = 40,   // a = (b == c)
    NeI64,        // a = (b != c)
    LtI64,        // a = (b < c)
    LeI64,        // a = (b <= c)
    GtI64,        // a = (b > c)
    GeI64,        // a = (b >= c)
    
    // ============ Comparison (f64) ============
    EqF64 = 50,
    NeF64,
    LtF64,
    LeF64,
    GtF64,
    GeF64,
    
    // ============ Reference comparison ============
    EqRef = 60,   // a = (b == c) for references
    NeRef,        // a = (b != c) for references
    IsNil,        // a = (b == nil)
    
    // ============ Bitwise ============
    Band = 70,    // a = b & c
    Bor,          // a = b | c
    Bxor,         // a = b ^ c
    Bnot,         // a = ^b
    Shl,          // a = b << c
    Shr,          // a = b >> c (arithmetic)
    Ushr,         // a = b >>> c (logical)
    
    // ============ Logical ============
    Not = 80,     // a = !b
    
    // ============ Control flow ============
    Jump = 90,    // pc += imm32(b,c)
    JumpIf,       // if a: pc += imm32(b,c)
    JumpIfNot,    // if !a: pc += imm32(b,c)
    
    // ============ Function call ============
    Call = 100,   // call func at a, args start at b, c=arg_count, flags=ret_count
    Return,       // return values starting at a, b=count
    
    // ============ Object operations ============
    Alloc = 110,  // a = new object of type b, c=extra_slots
    GetField,     // a = b.field[c]
    SetField,     // a.field[b] = c
    GetFieldN,    // copy c slots from b.field[flags] to a
    SetFieldN,    // copy c slots from b to a.field[flags]
    
    // ============ Array/Slice ============
    ArrayNew = 120,   // a = new array, elem_type=b, len=c
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
    StrNew = 140,     // a = new string from const b
    StrConcat,        // a = b + c (string concat)
    StrLen,           // a = len(b)
    StrIndex,         // a = b[c]
    
    // ============ Map ============
    MapNew = 150,     // a = make(map), key_type=b, val_type=c
    MapGet,           // a = map[b], ok stored at c
    MapSet,           // a[b] = c
    MapDelete,        // delete(a, b)
    MapLen,           // a = len(b)
    
    // ============ Channel ============
    ChanNew = 160,    // a = make(chan), elem_type=b, cap=c
    ChanSend,         // a <- b
    ChanRecv,         // a = <-b, ok at c
    ChanClose,        // close(a)
    
    // ============ Iterator (range) ============
    IterBegin = 170,  // begin iteration over a, type=b
    IterNext,         // a,b = next from iter, c=done_offset
    IterEnd,          // end iteration
    
    // ============ Closure ============
    ClosureNew = 180, // a = closure(func=b), c=upvalue_count
    ClosureGet,       // a = closure.upvalues[b]
    ClosureSet,       // closure.upvalues[a] = b
    ClosureCall,      // call closure at a, args at b, c=arg_count, flags=ret_count
    
    // ============ Goroutine ============
    Go = 190,         // go call func at a, args at b, c=arg_count
    Yield,            // yield current fiber
    
    // ============ Defer/Panic/Recover ============
    DeferPush = 200,  // defer func at a, args at b, c=arg_count
    DeferPop,         // pop and execute defers for current frame
    Panic,            // panic with value at a
    Recover,          // a = recover()
    
    // ============ Interface ============
    BoxInterface = 210,   // a = box(type=b, value=c) into interface
    UnboxInterface,       // a = unbox(b), type at c
    TypeAssert,           // a = b.(type c), ok at flags
    
    // ============ Type conversion ============
    I64ToF64 = 220,
    F64ToI64,
    I32ToI64,
    I64ToI32,
    
    // ============ Native call ============
    CallNative = 230, // call native func a, args at b, c=arg_count, flags=ret_count
    
    // ============ Debug ============
    DebugPrint = 250,
    
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
            
            20 => Self::AddI64,
            21 => Self::SubI64,
            22 => Self::MulI64,
            23 => Self::DivI64,
            24 => Self::ModI64,
            25 => Self::NegI64,
            
            30 => Self::AddF64,
            31 => Self::SubF64,
            32 => Self::MulF64,
            33 => Self::DivF64,
            34 => Self::NegF64,
            
            40 => Self::EqI64,
            41 => Self::NeI64,
            42 => Self::LtI64,
            43 => Self::LeI64,
            44 => Self::GtI64,
            45 => Self::GeI64,
            
            50 => Self::EqF64,
            51 => Self::NeF64,
            52 => Self::LtF64,
            53 => Self::LeF64,
            54 => Self::GtF64,
            55 => Self::GeF64,
            
            60 => Self::EqRef,
            61 => Self::NeRef,
            62 => Self::IsNil,
            
            70 => Self::Band,
            71 => Self::Bor,
            72 => Self::Bxor,
            73 => Self::Bnot,
            74 => Self::Shl,
            75 => Self::Shr,
            76 => Self::Ushr,
            
            80 => Self::Not,
            
            90 => Self::Jump,
            91 => Self::JumpIf,
            92 => Self::JumpIfNot,
            
            100 => Self::Call,
            101 => Self::Return,
            
            110 => Self::Alloc,
            111 => Self::GetField,
            112 => Self::SetField,
            113 => Self::GetFieldN,
            114 => Self::SetFieldN,
            
            120 => Self::ArrayNew,
            121 => Self::ArrayGet,
            122 => Self::ArraySet,
            123 => Self::ArrayLen,
            124 => Self::SliceNew,
            125 => Self::SliceGet,
            126 => Self::SliceSet,
            127 => Self::SliceLen,
            128 => Self::SliceCap,
            129 => Self::SliceSlice,
            130 => Self::SliceAppend,
            
            140 => Self::StrNew,
            141 => Self::StrConcat,
            142 => Self::StrLen,
            143 => Self::StrIndex,
            
            150 => Self::MapNew,
            151 => Self::MapGet,
            152 => Self::MapSet,
            153 => Self::MapDelete,
            154 => Self::MapLen,
            
            160 => Self::ChanNew,
            161 => Self::ChanSend,
            162 => Self::ChanRecv,
            163 => Self::ChanClose,
            
            170 => Self::IterBegin,
            171 => Self::IterNext,
            172 => Self::IterEnd,
            
            180 => Self::ClosureNew,
            181 => Self::ClosureGet,
            182 => Self::ClosureSet,
            183 => Self::ClosureCall,
            
            190 => Self::Go,
            191 => Self::Yield,
            
            200 => Self::DeferPush,
            201 => Self::DeferPop,
            202 => Self::Panic,
            203 => Self::Recover,
            
            210 => Self::BoxInterface,
            211 => Self::UnboxInterface,
            212 => Self::TypeAssert,
            
            220 => Self::I64ToF64,
            221 => Self::F64ToI64,
            222 => Self::I32ToI64,
            223 => Self::I64ToI32,
            
            230 => Self::CallNative,
            
            250 => Self::DebugPrint,
            
            _ => Self::Invalid,
        }
    }
}
