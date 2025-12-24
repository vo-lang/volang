//! Instruction format and opcodes.

/// 8-byte fixed instruction format.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    pub op: u8,
    pub flags: u8,
    pub a: u16,
    pub b: u16,
    pub c: u16,
}

impl Instruction {
    #[inline]
    pub const fn new(op: Opcode, a: u16, b: u16, c: u16) -> Self {
        Self {
            op: op as u8,
            flags: 0,
            a,
            b,
            c,
        }
    }

    #[inline]
    pub const fn with_flags(op: Opcode, flags: u8, a: u16, b: u16, c: u16) -> Self {
        Self {
            op: op as u8,
            flags,
            a,
            b,
            c,
        }
    }

    #[inline]
    pub fn opcode(&self) -> Opcode {
        Opcode::from_u8(self.op)
    }

    #[inline]
    pub fn imm32(&self) -> i32 {
        ((self.b as u32) | ((self.c as u32) << 16)) as i32
    }

    #[inline]
    pub fn imm32_unsigned(&self) -> u32 {
        (self.b as u32) | ((self.c as u32) << 16)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    // === LOAD: Load immediate/constant ===
    Nop = 0,
    LoadNil,
    LoadTrue,
    LoadFalse,
    LoadInt,
    LoadConst,

    // === COPY: Stack slot copy ===
    Copy,
    CopyN,

    // === SLOT: Stack dynamic indexing (for stack arrays) ===
    SlotGet,
    SlotSet,
    SlotGetN,
    SlotSetN,

    // === GLOBAL: Global variables ===
    GlobalGet,
    GlobalGetN,
    GlobalSet,
    GlobalSetN,

    // === PTR: Heap pointer operations ===
    PtrNew,
    PtrClone,
    PtrGet,
    PtrSet,
    PtrGetN,
    PtrSetN,

    // === ARITH: Integer arithmetic ===
    AddI,
    SubI,
    MulI,
    DivI,
    ModI,
    NegI,

    // === ARITH: Float arithmetic ===
    AddF,
    SubF,
    MulF,
    DivF,
    NegF,

    // === CMP: Integer comparison ===
    EqI,
    NeI,
    LtI,
    LeI,
    GtI,
    GeI,

    // === CMP: Float comparison ===
    EqF,
    NeF,
    LtF,
    LeF,
    GtF,
    GeF,

    // === CMP: Reference comparison ===
    EqRef,
    NeRef,
    IsNil,

    // === BIT: Bitwise operations ===
    And,
    Or,
    Xor,
    Not,
    Shl,
    ShrS,
    ShrU,

    // === LOGIC: Logical operations ===
    BoolNot,

    // === JUMP: Control flow ===
    Jump,
    JumpIf,
    JumpIfNot,

    // === CALL: Function calls ===
    Call,
    CallExtern,
    CallClosure,
    CallIface,
    Return,

    // === STR: String operations ===
    StrNew,
    StrLen,
    StrIndex,
    StrConcat,
    StrSlice,
    StrEq,
    StrNe,
    StrLt,
    StrLe,
    StrGt,
    StrGe,

    // === ARRAY: Heap array operations ===
    ArrayNew,
    ArrayGet,
    ArraySet,
    ArrayLen,

    // === SLICE: Slice operations ===
    SliceNew,
    SliceGet,
    SliceSet,
    SliceLen,
    SliceCap,
    SliceSlice,
    SliceAppend,

    // === MAP: Map operations ===
    MapNew,
    MapGet,
    MapSet,
    MapDelete,
    MapLen,

    // === CHAN: Channel operations ===
    ChanNew,
    ChanSend,
    ChanRecv,
    ChanClose,

    // === SELECT: Select statement ===
    SelectBegin,
    SelectSend,
    SelectRecv,
    SelectExec,

    // === ITER: Iterator (for-range) ===
    IterBegin,
    IterNext,
    IterEnd,

    // === CLOSURE: Closure operations ===
    ClosureNew,
    ClosureGet,
    ClosureSet,

    // === GO: Goroutine ===
    GoCall,
    Yield,

    // === DEFER: Defer and error handling ===
    DeferPush,
    ErrDeferPush,
    Panic,
    Recover,

    // === IFACE: Interface operations ===
    IfaceAssign,
    IfaceAssert,

    // === CONV: Type conversion ===
    ConvI2F,
    ConvF2I,
    ConvI32I64,
    ConvI64I32,

    // Sentinel for invalid/unknown opcodes
    Invalid = 255,
}

impl Opcode {
    #[inline]
    pub fn from_u8(v: u8) -> Self {
        match v {
            0 => Self::Nop,
            1 => Self::LoadNil,
            2 => Self::LoadTrue,
            3 => Self::LoadFalse,
            4 => Self::LoadInt,
            5 => Self::LoadConst,
            6 => Self::Copy,
            7 => Self::CopyN,
            8 => Self::SlotGet,
            9 => Self::SlotSet,
            10 => Self::SlotGetN,
            11 => Self::SlotSetN,
            12 => Self::GlobalGet,
            13 => Self::GlobalGetN,
            14 => Self::GlobalSet,
            15 => Self::GlobalSetN,
            16 => Self::PtrNew,
            17 => Self::PtrClone,
            18 => Self::PtrGet,
            19 => Self::PtrSet,
            20 => Self::PtrGetN,
            21 => Self::PtrSetN,
            22 => Self::AddI,
            23 => Self::SubI,
            24 => Self::MulI,
            25 => Self::DivI,
            26 => Self::ModI,
            27 => Self::NegI,
            28 => Self::AddF,
            29 => Self::SubF,
            30 => Self::MulF,
            31 => Self::DivF,
            32 => Self::NegF,
            33 => Self::EqI,
            34 => Self::NeI,
            35 => Self::LtI,
            36 => Self::LeI,
            37 => Self::GtI,
            38 => Self::GeI,
            39 => Self::EqF,
            40 => Self::NeF,
            41 => Self::LtF,
            42 => Self::LeF,
            43 => Self::GtF,
            44 => Self::GeF,
            45 => Self::EqRef,
            46 => Self::NeRef,
            47 => Self::IsNil,
            48 => Self::And,
            49 => Self::Or,
            50 => Self::Xor,
            51 => Self::Not,
            52 => Self::Shl,
            53 => Self::ShrS,
            54 => Self::ShrU,
            55 => Self::BoolNot,
            56 => Self::Jump,
            57 => Self::JumpIf,
            58 => Self::JumpIfNot,
            59 => Self::Call,
            60 => Self::CallExtern,
            61 => Self::CallClosure,
            62 => Self::CallIface,
            63 => Self::Return,
            64 => Self::StrNew,
            65 => Self::StrLen,
            66 => Self::StrIndex,
            67 => Self::StrConcat,
            68 => Self::StrSlice,
            69 => Self::StrEq,
            70 => Self::StrNe,
            71 => Self::StrLt,
            72 => Self::StrLe,
            73 => Self::StrGt,
            74 => Self::StrGe,
            75 => Self::ArrayNew,
            76 => Self::ArrayGet,
            77 => Self::ArraySet,
            78 => Self::ArrayLen,
            79 => Self::SliceNew,
            80 => Self::SliceGet,
            81 => Self::SliceSet,
            82 => Self::SliceLen,
            83 => Self::SliceCap,
            84 => Self::SliceSlice,
            85 => Self::SliceAppend,
            86 => Self::MapNew,
            87 => Self::MapGet,
            88 => Self::MapSet,
            89 => Self::MapDelete,
            90 => Self::MapLen,
            91 => Self::ChanNew,
            92 => Self::ChanSend,
            93 => Self::ChanRecv,
            94 => Self::ChanClose,
            95 => Self::SelectBegin,
            96 => Self::SelectSend,
            97 => Self::SelectRecv,
            98 => Self::SelectExec,
            99 => Self::IterBegin,
            100 => Self::IterNext,
            101 => Self::IterEnd,
            102 => Self::ClosureNew,
            103 => Self::ClosureGet,
            104 => Self::ClosureSet,
            105 => Self::GoCall,
            106 => Self::Yield,
            107 => Self::DeferPush,
            108 => Self::ErrDeferPush,
            109 => Self::Panic,
            110 => Self::Recover,
            111 => Self::IfaceAssign,
            112 => Self::IfaceAssert,
            113 => Self::ConvI2F,
            114 => Self::ConvF2I,
            115 => Self::ConvI32I64,
            116 => Self::ConvI64I32,
            _ => Self::Invalid,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instruction_size() {
        assert_eq!(core::mem::size_of::<Instruction>(), 8);
    }

    #[test]
    fn test_imm32() {
        let inst = Instruction::new(Opcode::LoadInt, 0, 0x1234, 0x5678);
        assert_eq!(inst.imm32_unsigned(), 0x56781234);
    }

    #[test]
    fn test_imm32_signed() {
        let inst = Instruction::new(Opcode::Jump, 0, 0xFFFF, 0xFFFF);
        assert_eq!(inst.imm32(), -1);
    }

    #[test]
    fn test_opcode_roundtrip() {
        for i in 0..=116u8 {
            let op = Opcode::from_u8(i);
            assert_ne!(op, Opcode::Invalid, "opcode {} should be valid", i);
            assert_eq!(op as u8, i);
        }
    }
}
