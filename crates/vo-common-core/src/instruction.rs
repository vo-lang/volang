//! Instruction format and opcodes.

// =============================================================================
// Hint instruction constants (for JIT loop analysis)
// =============================================================================

/// Hint type: pure NOP (backward compatible)
pub const HINT_NOP: u8 = 0;
/// Hint type: loop begin marker
pub const HINT_LOOP_BEGIN: u8 = 1;
/// Hint type: loop end marker
pub const HINT_LOOP_END: u8 = 2;
/// Hint type: loop metadata (e.g., continue_pc for labeled continue)
pub const HINT_LOOP_META: u8 = 3;

/// Loop flag: contains defer statement
pub const LOOP_FLAG_HAS_DEFER: u8 = 0x01;
/// Loop flag: has labeled break to outer loop
pub const LOOP_FLAG_HAS_LABELED_BREAK: u8 = 0x02;
/// Loop flag: has labeled continue to outer loop
pub const LOOP_FLAG_HAS_LABELED_CONTINUE: u8 = 0x04;

// =============================================================================
// Instruction format
// =============================================================================

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
    // === HINT: NOP / Loop metadata for JIT ===
    Hint = 0,
    // === LOAD: Load immediate/constant ===
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
    PtrGet,
    PtrSet,
    PtrGetN,
    PtrSetN,
    PtrAdd,  // a=dst, b=ptr, c=offset_slots (ptr arithmetic: dst = ptr + offset * 8)

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

    // === BIT: Bitwise operations ===
    And,
    Or,
    Xor,
    AndNot,  // a &^ b = a & (^b)
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
    StrDecodeRune,  // Decode UTF-8 rune at position: (rune, width) = decode(str, pos)

    // === ARRAY: Heap array operations ===
    ArrayNew,
    ArrayGet,
    ArraySet,
    /// Get element address: a=dst, b=array_gcref, c=index, flags=elem_bytes
    ArrayAddr,

    // === SLICE: Slice operations ===
    SliceNew,
    SliceGet,
    SliceSet,
    SliceLen,
    SliceCap,
    SliceSlice,
    SliceAppend,
    /// Get element address: a=dst, b=slice_reg, c=index, flags=elem_bytes
    SliceAddr,

    // === MAP: Map operations ===
    MapNew,
    MapGet,
    MapSet,
    MapDelete,
    MapLen,
    MapIterGet,  // Get key-value by index (for range expansion)

    // === CHAN: Channel operations ===
    ChanNew,
    ChanSend,
    ChanRecv,
    ChanClose,
    ChanLen,
    ChanCap,

    // === SELECT: Select statement ===
    SelectBegin,
    SelectSend,
    SelectRecv,
    SelectExec,


    // === CLOSURE: Closure operations ===
    ClosureNew,
    ClosureGet,

    // === GO: Goroutine ===
    /// GoStart: Start goroutine
    /// - a: func_id_low (if flags bit 0 = 0) or closure_reg (if flags bit 0 = 1)
    /// - b: args_start
    /// - c: arg_slots
    /// - flags bit 0: is_closure, bits 1-7: func_id_high (when not closure)
    GoStart,

    // === DEFER: Defer and error handling ===
    DeferPush,
    ErrDeferPush,
    Panic,
    Recover,

    // === IFACE: Interface operations ===
    IfaceAssign,
    IfaceAssert,
    /// Interface equality: a = (b == c) where b,c are 2-slot interfaces
    IfaceEq,

    // === CONV: Type conversion ===
    ConvI2F,
    ConvF2I,
    ConvF64F32,
    ConvF32F64,
    /// Truncate integer: a = truncate(b), flags = target width
    /// flags: 0x81=i8, 0x82=i16, 0x84=i32, 0x01=u8, 0x02=u16, 0x04=u32
    /// High bit (0x80) = signed (sign-extend), low bits = byte width
    Trunc,

    // Sentinel for invalid/unknown opcodes
    Invalid = 255,
}

impl Opcode {
    const MAX_VALID: u8 = Self::Trunc as u8;

    #[inline]
    pub fn from_u8(v: u8) -> Self {
        if v <= Self::MAX_VALID {
            // SAFETY: Opcode is #[repr(u8)] and v is within valid range
            unsafe { core::mem::transmute(v) }
        } else {
            Self::Invalid
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
        for i in 0..=Opcode::MAX_VALID {
            let op = Opcode::from_u8(i);
            assert_ne!(op, Opcode::Invalid, "opcode {} should be valid", i);
            assert_eq!(op as u8, i);
        }
    }
}
