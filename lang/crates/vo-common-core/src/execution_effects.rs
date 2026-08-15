//! Canonical bytecode execution effects shared by every execution backend.
//!
//! These facts describe language/runtime semantics, independent of how a
//! backend lowers an opcode. Verification, interpretation, and native code
//! generation must consume this table instead of maintaining parallel lists.

use crate::instruction::Opcode;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct EffectContract {
    pub may_gc: bool,
    pub may_alloc: bool,
    pub may_panic: bool,
    pub may_unwind: bool,
    pub may_call: bool,
    pub may_schedule: bool,
    pub may_observe_frame: bool,
    pub needs_frame: bool,
    pub needs_slot_metadata: bool,
    pub needs_type_metadata: bool,
    pub needs_write_barrier: bool,
    pub touches_interface: bool,
    pub materializes_closure: bool,
}

impl EffectContract {
    pub const PURE: Self = Self {
        may_gc: false,
        may_alloc: false,
        may_panic: false,
        may_unwind: false,
        may_call: false,
        may_schedule: false,
        may_observe_frame: false,
        needs_frame: false,
        needs_slot_metadata: false,
        needs_type_metadata: false,
        needs_write_barrier: false,
        touches_interface: false,
        materializes_closure: false,
    };

    pub const fn union(self, other: Self) -> Self {
        Self {
            may_gc: self.may_gc || other.may_gc,
            may_alloc: self.may_alloc || other.may_alloc,
            may_panic: self.may_panic || other.may_panic,
            may_unwind: self.may_unwind || other.may_unwind,
            may_call: self.may_call || other.may_call,
            may_schedule: self.may_schedule || other.may_schedule,
            may_observe_frame: self.may_observe_frame || other.may_observe_frame,
            needs_frame: self.needs_frame || other.needs_frame,
            needs_slot_metadata: self.needs_slot_metadata || other.needs_slot_metadata,
            needs_type_metadata: self.needs_type_metadata || other.needs_type_metadata,
            needs_write_barrier: self.needs_write_barrier || other.needs_write_barrier,
            touches_interface: self.touches_interface || other.touches_interface,
            materializes_closure: self.materializes_closure || other.materializes_closure,
        }
    }

    pub const fn permits_frame_elision(self) -> bool {
        !(self.may_panic
            || self.may_unwind
            || self.may_call
            || self.may_schedule
            || self.may_observe_frame
            || self.needs_frame
            || self.needs_write_barrier
            || self.touches_interface
            || self.materializes_closure)
    }

    /// A prepared call has precise shadow-stack slots, but no registered VM
    /// call frame until a non-OK result is materialized.
    pub const fn permits_prepared_shadow_frame(self) -> bool {
        !(self.may_unwind
            || self.may_call
            || self.may_schedule
            || self.may_observe_frame
            || self.needs_frame
            || self.materializes_closure)
    }
}

const C_PANIC: EffectContract = EffectContract {
    may_panic: true,
    ..EffectContract::PURE
};
const C_SLOT_META_PANIC: EffectContract = EffectContract {
    may_panic: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_PTR_SET: EffectContract = EffectContract {
    may_panic: true,
    needs_write_barrier: true,
    ..EffectContract::PURE
};
const C_INDEXED_SET: EffectContract = EffectContract {
    may_panic: true,
    needs_slot_metadata: true,
    needs_write_barrier: true,
    ..EffectContract::PURE
};
const C_ALLOC_TYPED: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    needs_type_metadata: true,
    ..EffectContract::PURE
};
const C_ALLOC_TYPED_PANIC: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    may_panic: true,
    needs_type_metadata: true,
    ..EffectContract::PURE
};
const C_ALLOC_TYPED_SLOT: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    needs_type_metadata: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_MAP_HELPER: EffectContract = EffectContract {
    needs_slot_metadata: true,
    needs_type_metadata: true,
    touches_interface: true,
    ..EffectContract::PURE
};
const C_MAP_PANIC: EffectContract = EffectContract {
    may_panic: true,
    needs_slot_metadata: true,
    needs_type_metadata: true,
    touches_interface: true,
    ..EffectContract::PURE
};
const C_MAP_SET: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    may_panic: true,
    needs_slot_metadata: true,
    needs_type_metadata: true,
    needs_write_barrier: true,
    touches_interface: true,
    ..EffectContract::PURE
};
const C_QUEUE_FRAME: EffectContract = EffectContract {
    may_gc: true,
    may_panic: true,
    may_schedule: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_QUEUE_GET_FRAME: EffectContract = EffectContract {
    may_panic: true,
    may_observe_frame: true,
    needs_frame: true,
    ..EffectContract::PURE
};
const C_GO_FRAME: EffectContract = EffectContract {
    may_gc: true,
    may_panic: true,
    may_call: true,
    may_schedule: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_CLOSURE_NEW: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    needs_slot_metadata: true,
    materializes_closure: true,
    ..EffectContract::PURE
};
const C_CALL: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    may_panic: true,
    may_unwind: true,
    may_call: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_CALL_EXTERN: EffectContract = EffectContract {
    may_schedule: true,
    ..C_CALL
};
const C_CALL_CLOSURE: EffectContract = EffectContract {
    materializes_closure: true,
    ..C_CALL
};
const C_CALL_IFACE: EffectContract = EffectContract {
    touches_interface: true,
    ..C_CALL
};
const C_DEFER: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    may_panic: true,
    may_unwind: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    materializes_closure: true,
    ..EffectContract::PURE
};
const C_RECOVER: EffectContract = EffectContract {
    may_gc: true,
    may_panic: true,
    may_unwind: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_PANIC_CONTROL: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    may_panic: true,
    may_unwind: true,
    may_observe_frame: true,
    needs_frame: true,
    needs_slot_metadata: true,
    ..EffectContract::PURE
};
const C_IFACE_ASSIGN: EffectContract = EffectContract {
    may_gc: true,
    may_alloc: true,
    needs_slot_metadata: true,
    needs_type_metadata: true,
    touches_interface: true,
    ..EffectContract::PURE
};
const C_IFACE_PANIC: EffectContract = EffectContract {
    may_panic: true,
    needs_slot_metadata: true,
    needs_type_metadata: true,
    touches_interface: true,
    ..EffectContract::PURE
};
const C_INVALID: EffectContract = EffectContract {
    may_panic: true,
    may_unwind: true,
    needs_frame: true,
    ..EffectContract::PURE
};

/// Return the canonical execution effects for one bytecode opcode.
///
/// The exhaustive match intentionally has no wildcard, so adding an opcode
/// requires an explicit semantic decision.
#[inline]
pub const fn opcode_effect_contract(opcode: Opcode) -> EffectContract {
    match opcode {
        Opcode::Hint
        | Opcode::LoadInt
        | Opcode::LoadConst
        | Opcode::Copy
        | Opcode::CopyN
        | Opcode::GlobalGet
        | Opcode::GlobalGetN
        | Opcode::GlobalSet
        | Opcode::GlobalSetN
        | Opcode::PtrAdd
        | Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::NegI
        | Opcode::AddF
        | Opcode::SubF
        | Opcode::MulF
        | Opcode::DivF
        | Opcode::NegF
        | Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LtU
        | Opcode::LeI
        | Opcode::LeU
        | Opcode::GtI
        | Opcode::GtU
        | Opcode::GeI
        | Opcode::GeU
        | Opcode::EqF
        | Opcode::NeF
        | Opcode::LtF
        | Opcode::LeF
        | Opcode::GtF
        | Opcode::GeF
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::Not
        | Opcode::BoolNot
        | Opcode::Jump
        | Opcode::JumpIf
        | Opcode::JumpIfNot
        | Opcode::Return
        | Opcode::StrLen
        | Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe
        | Opcode::StrDecodeRune
        | Opcode::SliceLen
        | Opcode::SliceCap
        | Opcode::MapLen
        | Opcode::ClosureGet
        | Opcode::ConvI2F
        | Opcode::ConvF2I
        | Opcode::ConvF64F32
        | Opcode::ConvF32F64
        | Opcode::Trunc
        | Opcode::ForLoop => EffectContract::PURE,

        Opcode::DivI
        | Opcode::DivU
        | Opcode::ModI
        | Opcode::ModU
        | Opcode::PtrSetN
        | Opcode::Shl
        | Opcode::ShrS
        | Opcode::ShrU
        | Opcode::StrIndex
        | Opcode::IndexCheck => C_PANIC,

        Opcode::SlotGet
        | Opcode::SlotSet
        | Opcode::SlotGetN
        | Opcode::SlotSetN
        | Opcode::PtrGet
        | Opcode::PtrGetN
        | Opcode::ArrayGet
        | Opcode::ArrayAddr
        | Opcode::SliceGet
        | Opcode::SliceAddr => C_SLOT_META_PANIC,
        Opcode::PtrSet => C_PTR_SET,
        Opcode::ArraySet | Opcode::SliceSet => C_INDEXED_SET,
        Opcode::StrNew | Opcode::StrConcat | Opcode::MapNew | Opcode::IslandNew => C_ALLOC_TYPED,
        Opcode::StrSlice
        | Opcode::ArrayNew
        | Opcode::SliceNew
        | Opcode::SliceSlice
        | Opcode::QueueNew => C_ALLOC_TYPED_PANIC,
        Opcode::PtrNew | Opcode::SliceAppend => C_ALLOC_TYPED_SLOT,
        Opcode::MapIterInit | Opcode::MapIterNext => C_MAP_HELPER,
        Opcode::MapGet | Opcode::MapDelete => C_MAP_PANIC,
        Opcode::MapSet => C_MAP_SET,
        Opcode::QueueSend
        | Opcode::QueueRecv
        | Opcode::QueueClose
        | Opcode::SelectBegin
        | Opcode::SelectSend
        | Opcode::SelectRecv
        | Opcode::SelectExec => C_QUEUE_FRAME,
        Opcode::QueueLen | Opcode::QueueCap => C_QUEUE_GET_FRAME,
        Opcode::GoStart | Opcode::GoIsland => C_GO_FRAME,
        Opcode::ClosureNew => C_CLOSURE_NEW,
        Opcode::Call => C_CALL,
        Opcode::CallExtern => C_CALL_EXTERN,
        Opcode::CallClosure => C_CALL_CLOSURE,
        Opcode::CallIface => C_CALL_IFACE,
        Opcode::DeferPush | Opcode::ErrDeferPush => C_DEFER,
        Opcode::Panic => C_PANIC_CONTROL,
        Opcode::Recover => C_RECOVER,
        Opcode::IfaceAssign => C_IFACE_ASSIGN,
        Opcode::IfaceAssert | Opcode::IfaceEq => C_IFACE_PANIC,
        Opcode::Invalid => C_INVALID,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_valid_opcode_has_a_contract() {
        for raw in 0..Opcode::COUNT {
            let opcode = Opcode::from_u8(raw as u8);
            assert_ne!(opcode, Opcode::Invalid);
            let _ = opcode_effect_contract(opcode);
        }
    }

    #[test]
    fn allocation_contract_covers_direct_managed_allocators() {
        for opcode in [
            Opcode::PtrNew,
            Opcode::StrNew,
            Opcode::StrConcat,
            Opcode::ArrayNew,
            Opcode::SliceNew,
            Opcode::SliceAppend,
            Opcode::MapNew,
            Opcode::MapSet,
            Opcode::QueueNew,
            Opcode::ClosureNew,
            Opcode::DeferPush,
            Opcode::ErrDeferPush,
            Opcode::IfaceAssign,
            Opcode::IslandNew,
        ] {
            let effects = opcode_effect_contract(opcode);
            assert!(effects.may_alloc, "{opcode:?}");
            assert!(effects.may_gc, "{opcode:?}");
        }
    }
}
