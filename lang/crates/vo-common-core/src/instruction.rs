//! Instruction format and opcodes.

// =============================================================================
// Hint instruction constants (for JIT loop analysis)
// =============================================================================

/// Hint type: pure NOP
pub const HINT_NOP: u8 = 0;
/// Hint type: loop marker (encodes depth, end_offset, flags, exit_pc)
pub const HINT_LOOP: u8 = 1;

/// Loop flag: contains defer statement
pub const LOOP_FLAG_HAS_DEFER: u8 = 0x01;
/// Loop flag: has labeled break to outer loop
pub const LOOP_FLAG_HAS_LABELED_BREAK: u8 = 0x02;
/// Loop flag: has labeled continue to outer loop
pub const LOOP_FLAG_HAS_LABELED_CONTINUE: u8 = 0x04;
pub const QUEUE_KIND_PORT_FLAG: u8 = 0x80;
pub const QUEUE_NEW_MAX_ELEM_SLOTS: u16 = 0x7F;
pub const QUEUE_SEND_MAX_ELEM_SLOTS: u16 = 0xFF;
pub const QUEUE_RECV_MAX_ELEM_SLOTS: u16 = 0x7F;
pub const CALL_SHAPE_MAX_ARG_RET_SLOTS: u16 = 0xFF;
pub const MAP_NEW_MAX_KEY_VAL_SLOTS: u16 = 0xFF;
pub const MAP_SET_MAX_KEY_VAL_SLOTS: u16 = 0xFF;
pub const MAP_GET_MAX_VALUE_SLOTS: u16 = 0x7FFF;
pub const MAP_ITER_MAX_KEY_VAL_SLOTS: u16 = 0x0F;
pub const MAP_ITER_METADATA_WIDTH_SENTINEL: u8 = 0;
pub const IFACE_ASSERT_MAX_ASSERT_KIND: u8 = 0x01;
pub const IFACE_ASSERT_MAX_TARGET_SLOTS: u16 = 0x1F;

#[inline]
pub const fn pack_u8_slot_count(slots: u16) -> Option<u8> {
    if slots <= u8::MAX as u16 {
        Some(slots as u8)
    } else {
        None
    }
}

#[inline]
pub const fn copy_n_mirror_flags(count: u16) -> u8 {
    if count <= u8::MAX as u16 {
        count as u8
    } else {
        0
    }
}

#[inline]
pub const fn pack_queue_new_flags(elem_slots: u16, is_port: bool) -> Option<u8> {
    if elem_slots <= QUEUE_NEW_MAX_ELEM_SLOTS {
        let slots = elem_slots as u8;
        if is_port {
            Some(slots | QUEUE_KIND_PORT_FLAG)
        } else {
            Some(slots)
        }
    } else {
        None
    }
}

#[inline]
pub const fn pack_queue_abi_elem_slots(elem_slots: u16) -> Option<u8> {
    if elem_slots <= QUEUE_NEW_MAX_ELEM_SLOTS {
        Some(elem_slots as u8)
    } else {
        None
    }
}

#[inline]
pub const fn pack_queue_send_flags(elem_slots: u16) -> Option<u8> {
    if elem_slots <= QUEUE_SEND_MAX_ELEM_SLOTS {
        Some(elem_slots as u8)
    } else {
        None
    }
}

#[inline]
pub const fn pack_queue_recv_flags(elem_slots: u16, has_ok: bool) -> Option<u8> {
    if elem_slots <= QUEUE_RECV_MAX_ELEM_SLOTS {
        Some(((elem_slots as u8) << 1) | has_ok as u8)
    } else {
        None
    }
}

#[inline]
pub const fn pack_map_iter_next_flags(key_slots: u16, val_slots: u16) -> Option<u8> {
    if key_slots <= MAP_ITER_MAX_KEY_VAL_SLOTS && val_slots <= MAP_ITER_MAX_KEY_VAL_SLOTS {
        Some((key_slots as u8) | ((val_slots as u8) << 4))
    } else {
        Some(MAP_ITER_METADATA_WIDTH_SENTINEL)
    }
}

#[inline]
pub const fn pack_map_new_slots(key_slots: u16, val_slots: u16) -> Option<u16> {
    if key_slots <= MAP_NEW_MAX_KEY_VAL_SLOTS && val_slots <= MAP_NEW_MAX_KEY_VAL_SLOTS {
        Some((key_slots << 8) | val_slots)
    } else {
        None
    }
}

#[inline]
pub const fn pack_map_set_meta(key_slots: u16, val_slots: u16) -> Option<u32> {
    if key_slots <= MAP_SET_MAX_KEY_VAL_SLOTS && val_slots <= MAP_SET_MAX_KEY_VAL_SLOTS {
        Some(((key_slots as u32) << 8) | (val_slots as u32))
    } else {
        None
    }
}

#[inline]
pub const fn pack_map_get_meta(key_slots: u16, val_slots: u16, has_ok: bool) -> Option<u32> {
    if val_slots <= MAP_GET_MAX_VALUE_SLOTS {
        Some(((key_slots as u32) << 16) | ((val_slots as u32) << 1) | (has_ok as u32))
    } else {
        None
    }
}

#[inline]
pub const fn pack_call_shape(arg_slots: u16, ret_slots: u16) -> Option<u16> {
    if arg_slots <= CALL_SHAPE_MAX_ARG_RET_SLOTS && ret_slots <= CALL_SHAPE_MAX_ARG_RET_SLOTS {
        Some((arg_slots << 8) | ret_slots)
    } else {
        None
    }
}

#[inline]
pub const fn pack_iface_assert_flags(
    assert_kind: u8,
    has_ok: bool,
    target_slots: u16,
) -> Option<u8> {
    if assert_kind <= IFACE_ASSERT_MAX_ASSERT_KIND && target_slots <= IFACE_ASSERT_MAX_TARGET_SLOTS
    {
        if assert_kind == 1 && target_slots != 2 {
            return None;
        }
        Some(assert_kind | ((has_ok as u8) << 2) | ((target_slots as u8) << 3))
    } else {
        None
    }
}

#[inline]
pub const fn iface_assert_result_slots_from_flags(flags: u16) -> Option<u16> {
    let assert_kind = flags & 0x3;
    let target_slots = flags >> 3;
    if target_slots > IFACE_ASSERT_MAX_TARGET_SLOTS {
        return None;
    }
    match assert_kind {
        0 => {
            if target_slots == 0 {
                Some(1)
            } else {
                Some(target_slots)
            }
        }
        1 => {
            if target_slots == 2 {
                Some(2)
            } else {
                None
            }
        }
        _ => None,
    }
}

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

    /// ForLoop target PC calculation.
    ///
    /// ForLoop stores offset relative to (pc+1), so target = pc + 1 + offset.
    /// This matches the VM where frame.pc is incremented before dispatch.
    #[inline]
    pub fn forloop_target(&self, current_pc: usize) -> usize {
        let offset = self.c as i16;
        (current_pc as i32 + 1 + offset as i32) as usize
    }

    /// ForLoop flags: (is_decrement, is_unsigned)
    /// - bit 0: unsigned comparison (default: signed)
    /// - bit 1: decrement (default: increment)
    /// - bit 2: inclusive comparison (default: exclusive)
    #[inline]
    pub fn forloop_flags(&self) -> (bool, bool, bool) {
        let is_decrement = (self.flags & 0x02) != 0;
        let is_unsigned = (self.flags & 0x01) != 0;
        let is_inclusive = (self.flags & 0x04) != 0;
        (is_decrement, is_unsigned, is_inclusive)
    }

    /// Number of slots copied by CopyN.
    ///
    /// The canonical encoding stores the count in `c`; `flags` is only a
    /// non-authoritative mirror for small counts.
    #[inline]
    pub fn copy_n_count(&self) -> u16 {
        assert!(
            self.c != 0 || self.flags == 0,
            "CopyN count must be encoded in c"
        );
        self.c
    }

    /// Static Call target function id.
    ///
    /// `Call` stores the low 16 bits in `a` and the high 8 bits in `flags`.
    #[inline]
    pub const fn static_call_func_id(&self) -> u32 {
        (self.a as u32) | ((self.flags as u32) << 16)
    }

    /// ClosureNew target function id.
    ///
    /// `ClosureNew` stores the low 16 bits in `b` and the high 8 bits in `flags`.
    #[inline]
    pub const fn closure_new_func_id(&self) -> u32 {
        (self.b as u32) | ((self.flags as u32) << 16)
    }

    /// Whether the opcode's shared call/defer/go shape targets a closure value.
    ///
    /// Used by `GoStart`, `DeferPush`, and `ErrDeferPush`.
    #[inline]
    pub const fn call_shape_is_closure(&self) -> bool {
        (self.flags & 1) != 0
    }

    /// Static function id for the shared GoStart/DeferPush shape.
    ///
    /// The low 16 bits live in `a`; the high 7 bits live in `flags >> 1`
    /// because bit 0 is the closure marker.
    #[inline]
    pub const fn call_shape_static_func_id(&self) -> u32 {
        (self.a as u32) | (((self.flags as u32) >> 1) << 16)
    }

    /// Argument slot count for `Call`, `CallClosure`, and `CallIface`.
    #[inline]
    pub const fn packed_arg_slots(&self) -> u16 {
        self.c >> 8
    }

    /// Return slot count for `Call`, `CallClosure`, and `CallIface`.
    #[inline]
    pub const fn packed_ret_slots(&self) -> u16 {
        self.c & 0x00FF
    }

    /// Return destination for `Call`, `CallClosure`, and `CallIface`.
    #[inline]
    pub const fn packed_call_ret_start(&self) -> u16 {
        self.b + self.packed_arg_slots()
    }

    /// Key slot count for MapNew's packed `c` operand.
    #[inline]
    pub const fn map_new_key_slots(&self) -> u16 {
        self.c >> 8
    }

    /// Value slot count for MapNew's packed `c` operand.
    #[inline]
    pub const fn map_new_val_slots(&self) -> u16 {
        self.c & 0x00FF
    }

    /// Element slots for QueueNew flags.
    #[inline]
    pub const fn queue_new_elem_slots(&self) -> u16 {
        (self.flags & !QUEUE_KIND_PORT_FLAG) as u16
    }

    /// Whether QueueNew creates an island port instead of a local channel.
    #[inline]
    pub const fn queue_new_is_port(&self) -> bool {
        (self.flags & QUEUE_KIND_PORT_FLAG) != 0
    }

    /// Element slot count for QueueRecv/SelectRecv flags.
    #[inline]
    pub const fn recv_elem_slots(&self) -> u16 {
        ((self.flags >> 1) & 0x7F) as u16
    }

    /// Whether QueueRecv/SelectRecv writes an ok result slot.
    #[inline]
    pub const fn recv_has_ok(&self) -> bool {
        (self.flags & 1) != 0
    }

    /// MapIterNext key slot count.
    #[inline]
    pub const fn map_iter_key_slots(&self) -> u16 {
        (self.flags & 0x0F) as u16
    }

    /// MapIterNext value slot count.
    #[inline]
    pub const fn map_iter_val_slots(&self) -> u16 {
        ((self.flags >> 4) & 0x0F) as u16
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
    /// PtrNew: a=dst, b=meta register, c=slot count, flags=reserved.
    PtrNew,
    PtrGet,
    PtrSet,
    PtrGetN,
    PtrSetN,
    PtrAdd, // a=dst, b=ptr, c=offset_slots (ptr arithmetic: dst = ptr + offset * 8)

    // === ARITH: Integer arithmetic ===
    AddI,
    SubI,
    MulI,
    DivI,
    DivU,
    ModI,
    ModU,
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
    LtU,
    LeI,
    LeU,
    GtI,
    GtU,
    GeI,
    GeU,

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
    AndNot, // a &^ b = a & (^b)
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
    StrDecodeRune, // Decode UTF-8 rune at position: (rune, width) = decode(str, pos)

    // === ARRAY: Heap array operations ===
    ArrayNew,
    ArrayGet,
    ArraySet,
    /// Get element address: a=dst, b=array_gcref, c=index or index+elem_bytes, flags=elem layout
    ArrayAddr,

    // === SLICE: Slice operations ===
    SliceNew,
    SliceGet,
    SliceSet,
    SliceLen,
    SliceCap,
    SliceSlice,
    SliceAppend,
    /// Get element address: a=dst, b=slice_reg, c=index or index+elem_bytes, flags=elem layout
    SliceAddr,

    // === MAP: Map operations ===
    MapNew,
    MapGet,
    MapSet,
    MapDelete,
    MapLen,
    /// MapIterInit: Initialize map iterator
    /// a=iter_slot (7 slots), b=map_reg
    MapIterInit,
    /// MapIterNext: Advance iterator and get next key-value
    /// a=key_slot, b=iter_slot, flags=key_slots|(val_slots<<4)
    /// Sets zero flag if iterator exhausted
    MapIterNext,

    // === QUEUE: Channel/Port operations ===
    QueueNew,
    QueueSend,
    QueueRecv,
    QueueClose,
    QueueLen,
    QueueCap,

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

    /// Index bounds check: panic if a >= b (unsigned comparison)
    /// a = index_reg, b = len_reg
    IndexCheck,

    // === ISLAND: Island operations ===
    /// Create a new island (VM instance)
    /// a = dst
    IslandNew,

    /// Start goroutine on specific island
    /// a = island, b = closure, flags = capture_slots
    GoIsland,

    /// ForLoop: idx++; if idx < limit goto offset
    /// a = idx_slot, b = limit_slot, c = jump_offset (signed 16-bit, relative to pc+1)
    /// flags: bit0 = unsigned (0=signed), bit1 = decrement (0=increment)
    ForLoop,

    // Sentinel for invalid/unknown opcodes
    Invalid = 255,
}

impl Opcode {
    const MAX_VALID: u8 = Self::ForLoop as u8;
    /// Number of valid opcodes, excluding the `Invalid` sentinel.
    pub const COUNT: usize = Self::MAX_VALID as usize + 1;

    #[inline]
    pub fn from_u8(v: u8) -> Self {
        if v <= Self::MAX_VALID {
            // SAFETY: Opcode is #[repr(u8)] and v is within valid range
            unsafe { core::mem::transmute::<u8, Opcode>(v) }
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

    #[test]
    fn test_complex_instruction_accessors() {
        let call = Instruction::with_flags(Opcode::Call, 0x12, 0x3456, 10, (3 << 8) | 2);
        assert_eq!(call.static_call_func_id(), 0x12_3456);
        assert_eq!(call.packed_arg_slots(), 3);
        assert_eq!(call.packed_ret_slots(), 2);
        assert_eq!(call.packed_call_ret_start(), 13);

        let map_new = Instruction::new(Opcode::MapNew, 1, 2, (4 << 8) | 7);
        assert_eq!(map_new.map_new_key_slots(), 4);
        assert_eq!(map_new.map_new_val_slots(), 7);

        let closure_new = Instruction::with_flags(Opcode::ClosureNew, 0xAB, 1, 0xCDEF, 4);
        assert_eq!(closure_new.closure_new_func_id(), 0xAB_CDEF);

        let go = Instruction::with_flags(Opcode::GoStart, 0x24, 0x1000, 4, 2);
        assert!(!go.call_shape_is_closure());
        assert_eq!(go.call_shape_static_func_id(), 0x12_1000);

        let queue = Instruction::with_flags(Opcode::QueueNew, QUEUE_KIND_PORT_FLAG | 3, 1, 2, 3);
        assert!(queue.queue_new_is_port());
        assert_eq!(queue.queue_new_elem_slots(), 3);

        let recv = Instruction::with_flags(Opcode::SelectRecv, (4 << 1) | 1, 1, 2, 3);
        assert_eq!(recv.recv_elem_slots(), 4);
        assert!(recv.recv_has_ok());

        let iter = Instruction::with_flags(Opcode::MapIterNext, (5 << 4) | 2, 1, 2, 3);
        assert_eq!(iter.map_iter_key_slots(), 2);
        assert_eq!(iter.map_iter_val_slots(), 5);
    }

    #[test]
    fn packed_operand_helpers_reject_truncating_slot_counts() {
        assert_eq!(pack_queue_new_flags(127, false), Some(127));
        assert_eq!(
            pack_queue_new_flags(127, true),
            Some(QUEUE_KIND_PORT_FLAG | 127)
        );
        assert_eq!(pack_queue_new_flags(128, false), None);
        assert_eq!(pack_queue_send_flags(255), Some(255));
        assert_eq!(pack_queue_send_flags(256), None);
        assert_eq!(pack_queue_recv_flags(127, true), Some((127 << 1) | 1));
        assert_eq!(pack_queue_recv_flags(128, false), None);
        assert_eq!(pack_call_shape(255, 255), Some(0xFFFF));
        assert_eq!(pack_call_shape(256, 1), None);
        assert_eq!(pack_call_shape(1, 256), None);
        assert_eq!(pack_map_new_slots(255, 255), Some(0xFFFF));
        assert_eq!(pack_map_new_slots(256, 1), None);
        assert_eq!(pack_map_new_slots(1, 256), None);
        assert_eq!(pack_map_set_meta(255, 255), Some(0xFFFF));
        assert_eq!(pack_map_set_meta(256, 1), None);
        assert_eq!(pack_map_set_meta(1, 256), None);
        assert_eq!(pack_map_get_meta(u16::MAX, 0x7FFF, true), Some(u32::MAX));
        assert_eq!(pack_map_get_meta(1, 0x8000, false), None);
        assert_eq!(pack_map_iter_next_flags(15, 15), Some(0xFF));
        assert_eq!(
            pack_map_iter_next_flags(16, 1),
            Some(MAP_ITER_METADATA_WIDTH_SENTINEL)
        );
        assert_eq!(
            pack_map_iter_next_flags(1, 16),
            Some(MAP_ITER_METADATA_WIDTH_SENTINEL)
        );
        assert_eq!(
            pack_iface_assert_flags(1, true, 2),
            Some(1 | (1 << 2) | (2 << 3))
        );
        assert_eq!(pack_iface_assert_flags(1, false, 1), None);
        assert_eq!(pack_iface_assert_flags(1, false, 31), None);
        assert_eq!(pack_iface_assert_flags(2, false, 1), None);
        assert_eq!(pack_iface_assert_flags(3, false, 1), None);
        assert_eq!(pack_iface_assert_flags(4, false, 1), None);
        assert_eq!(pack_iface_assert_flags(1, false, 32), None);
        assert_eq!(iface_assert_result_slots_from_flags(2 << 3), Some(2));
        assert_eq!(iface_assert_result_slots_from_flags(1 | (1 << 3)), None);
        assert_eq!(iface_assert_result_slots_from_flags(1 | (2 << 3)), Some(2));
        assert_eq!(pack_u8_slot_count(255), Some(255));
        assert_eq!(pack_u8_slot_count(256), None);
        assert_eq!(copy_n_mirror_flags(255), 255);
        assert_eq!(copy_n_mirror_flags(256), 0);
    }
}
