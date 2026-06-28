use crate::semantics::FailFastCondition;

use super::types::{FieldDomain, FieldWidth, LayoutAuthority, SlotLayoutField};

pub(super) const FF_NONE: &[FailFastCondition] = &[];
pub(super) const FF_LAYOUT: &[FailFastCondition] = &[FailFastCondition::LayoutMismatch];
pub(super) const FF_METADATA_LAYOUT: &[FailFastCondition] = &[
    FailFastCondition::MissingMetadata,
    FailFastCondition::LayoutMismatch,
];
pub(super) const FF_HELPER: &[FailFastCondition] = &[
    FailFastCondition::MissingHelper,
    FailFastCondition::CallbackError,
];
pub(super) const FF_CALLBACK: &[FailFastCondition] = &[
    FailFastCondition::MissingCallback,
    FailFastCondition::CallbackError,
];
pub(super) const FF_GC_FRAME: &[FailFastCondition] = &[
    FailFastCondition::LayoutMismatch,
    FailFastCondition::GcFrameContract,
];

pub(super) const FIELD_IMM32: &[FieldWidth] = &[FieldWidth {
    name: "b:c",
    bits: 32,
    max: None,
    domain: FieldDomain::Any,
    authority: LayoutAuthority::InstructionOperand,
}];
pub(super) const FIELD_COPY_N: &[FieldWidth] = &[
    FieldWidth {
        name: "c.count",
        bits: 16,
        max: None,
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "flags.mirror",
        bits: 8,
        max: Some(u8::MAX as u16),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
];
pub(super) const FIELD_STATIC_FUNC: &[FieldWidth] = &[
    FieldWidth {
        name: "a.func_low",
        bits: 16,
        max: None,
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "flags.func_high",
        bits: 8,
        max: None,
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
];
pub(super) const FIELD_PACKED_CALL: &[FieldWidth] = &[
    FieldWidth {
        name: "c.arg_slots",
        bits: 8,
        max: Some(u8::MAX as u16),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "c.ret_slots",
        bits: 8,
        max: Some(u8::MAX as u16),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
];
pub(super) const FIELD_CALL_EXTERN_ARG_SLOTS: &[FieldWidth] = &[FieldWidth {
    name: "flags.arg_slots",
    bits: 8,
    max: Some(vo_runtime::instruction::CALL_SHAPE_MAX_ARG_RET_SLOTS),
    domain: FieldDomain::Any,
    authority: LayoutAuthority::InstructionFlags,
}];
pub(super) const FIELD_CALL_IFACE_METHOD_INDEX: &[FieldWidth] = &[FieldWidth {
    name: "flags.method_index",
    bits: 8,
    max: Some(u8::MAX as u16),
    domain: FieldDomain::Any,
    authority: LayoutAuthority::InstructionFlags,
}];
pub(super) const FIELD_CLOSURE_FUNC: &[FieldWidth] = &[
    FieldWidth {
        name: "b.func_low",
        bits: 16,
        max: None,
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "flags.func_high",
        bits: 8,
        max: None,
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
];
pub(super) const FIELD_SHARED_CALL: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.is_closure",
        bits: 1,
        max: Some(1),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "a/flags.static_func_id",
        bits: 23,
        max: None,
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperandAndFlags,
    },
    FieldWidth {
        name: "c.arg_slots",
        bits: 16,
        max: None,
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
];
pub(super) const FIELD_GO_ISLAND_ARG_SLOTS: &[FieldWidth] = &[FieldWidth {
    name: "flags.arg_slots",
    bits: 8,
    max: Some(vo_runtime::instruction::CALL_SHAPE_MAX_ARG_RET_SLOTS),
    domain: FieldDomain::Any,
    authority: LayoutAuthority::InstructionFlags,
}];
pub(super) const FIELD_MAP_NEW: &[FieldWidth] = &[
    FieldWidth {
        name: "c.key_slots",
        bits: 8,
        max: Some(u8::MAX as u16),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "c.val_slots",
        bits: 8,
        max: Some(u8::MAX as u16),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
];
pub(super) const FIELD_QUEUE_NEW: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.elem_slots",
        bits: 7,
        max: Some(vo_runtime::instruction::QUEUE_NEW_MAX_ELEM_SLOTS),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.kind",
        bits: 1,
        max: Some(1),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
];
pub(super) const FIELD_QUEUE_SEND: &[FieldWidth] = &[FieldWidth {
    name: "flags.elem_slots",
    bits: 8,
    max: Some(vo_runtime::instruction::QUEUE_SEND_MAX_ELEM_SLOTS),
    domain: FieldDomain::Any,
    authority: LayoutAuthority::InstructionFlags,
}];
pub(super) const FIELD_RECV: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.elem_slots",
        bits: 7,
        max: Some(vo_runtime::instruction::QUEUE_RECV_MAX_ELEM_SLOTS),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.has_ok",
        bits: 1,
        max: Some(1),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
];
pub(super) const FIELD_MAP_ITER: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.key_slots",
        bits: 4,
        max: Some(vo_runtime::instruction::MAP_ITER_MAX_KEY_VAL_SLOTS),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.val_slots",
        bits: 4,
        max: Some(vo_runtime::instruction::MAP_ITER_MAX_KEY_VAL_SLOTS),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
];
pub(super) const FIELD_IFACE_ASSERT: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.assert_kind",
        bits: 2,
        max: Some(vo_runtime::instruction::IFACE_ASSERT_MAX_ASSERT_KIND as u16),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.has_ok",
        bits: 1,
        max: Some(1),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.target_slots",
        bits: 5,
        max: Some(vo_runtime::instruction::IFACE_ASSERT_MAX_TARGET_SLOTS),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
];
pub(super) const FIELD_TRUNC: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.signed",
        bits: 1,
        max: Some(1),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.bytes",
        bits: 7,
        max: Some(4),
        domain: FieldDomain::AllowedValues(&[1, 2, 4]),
        authority: LayoutAuthority::InstructionFlags,
    },
];
pub(super) const FIELD_RETURN: &[FieldWidth] = &[FieldWidth {
    name: "flags.allowed_mask",
    bits: 8,
    max: Some(vo_runtime::bytecode::ReturnFlags::ALLOWED_BITS as u16),
    domain: FieldDomain::AllowedMask(vo_runtime::bytecode::ReturnFlags::ALLOWED_BITS as u16),
    authority: LayoutAuthority::InstructionFlags,
}];
pub(super) const FIELD_FOR_LOOP: &[FieldWidth] = &[FieldWidth {
    name: "c.relative_i16",
    bits: 16,
    max: None,
    domain: FieldDomain::Any,
    authority: LayoutAuthority::InstructionOperand,
}];
pub(super) const FIELD_FOR_LOOP_FLAGS: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.unsigned",
        bits: 1,
        max: Some(1),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.decrement",
        bits: 1,
        max: Some(1),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "flags.inclusive",
        bits: 1,
        max: Some(1),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionFlags,
    },
];
pub(super) const FIELD_HINT_LOOP: &[FieldWidth] = &[
    FieldWidth {
        name: "flags.kind",
        bits: 8,
        max: Some(vo_runtime::instruction::HINT_LOOP as u16),
        domain: FieldDomain::AllowedValues(&[
            vo_runtime::instruction::HINT_NOP as u16,
            vo_runtime::instruction::HINT_LOOP as u16,
        ]),
        authority: LayoutAuthority::InstructionFlags,
    },
    FieldWidth {
        name: "a.loop_flags",
        bits: 4,
        max: Some(
            (vo_runtime::instruction::LOOP_FLAG_HAS_DEFER
                | vo_runtime::instruction::LOOP_FLAG_HAS_LABELED_BREAK
                | vo_runtime::instruction::LOOP_FLAG_HAS_LABELED_CONTINUE) as u16,
        ),
        domain: FieldDomain::AllowedMask(
            (vo_runtime::instruction::LOOP_FLAG_HAS_DEFER
                | vo_runtime::instruction::LOOP_FLAG_HAS_LABELED_BREAK
                | vo_runtime::instruction::LOOP_FLAG_HAS_LABELED_CONTINUE) as u16,
        ),
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "a.depth",
        bits: 4,
        max: Some(0x0F),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "a.end_offset",
        bits: 8,
        max: Some(u8::MAX as u16),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
    FieldWidth {
        name: "b:c.exit_pc",
        bits: 32,
        max: None,
        domain: FieldDomain::Any,
        authority: LayoutAuthority::InstructionOperand,
    },
];

pub(super) const FIELD_META_ELEM: &[FieldWidth] = &[
    FieldWidth {
        name: "elem_bytes",
        bits: 32,
        max: None,
        domain: FieldDomain::Any,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
    FieldWidth {
        name: "needs_sign_extend",
        bits: 1,
        max: Some(1),
        domain: FieldDomain::Any,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
];
pub(super) const FIELD_META_ELEM_LAYOUTS: &[SlotLayoutField] = &[SlotLayoutField {
    name: "slot_layout",
    count_bits: 32,
    slot_type_bits: 8,
    authority: LayoutAuthority::CodegenTypeInfo,
}];
pub(super) const FIELD_META_MAP_GET_SCALARS: &[FieldWidth] = &[FieldWidth {
    name: "has_ok",
    bits: 1,
    max: Some(1),
    domain: FieldDomain::Any,
    authority: LayoutAuthority::CodegenTypeInfo,
}];
pub(super) const FIELD_META_MAP_GET_LAYOUTS: &[SlotLayoutField] = &[
    SlotLayoutField {
        name: "key_layout",
        count_bits: 32,
        slot_type_bits: 8,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
    SlotLayoutField {
        name: "val_layout",
        count_bits: 32,
        slot_type_bits: 8,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
];
pub(super) const FIELD_META_MAP_SET_LAYOUTS: &[SlotLayoutField] = FIELD_META_MAP_GET_LAYOUTS;
pub(super) const FIELD_META_MAP_DELETE_LAYOUTS: &[SlotLayoutField] = &[SlotLayoutField {
    name: "key_layout",
    count_bits: 32,
    slot_type_bits: 8,
    authority: LayoutAuthority::CodegenTypeInfo,
}];
pub(super) const FIELD_META_PTR_LAYOUTS: &[SlotLayoutField] = &[SlotLayoutField {
    name: "value_layout",
    count_bits: 32,
    slot_type_bits: 8,
    authority: LayoutAuthority::CodegenTypeInfo,
}];
pub(super) const FIELD_META_SLOT_LAYOUTS: &[SlotLayoutField] = &[SlotLayoutField {
    name: "elem_layout",
    count_bits: 32,
    slot_type_bits: 8,
    authority: LayoutAuthority::CodegenTypeInfo,
}];
pub(super) const FIELD_META_CALL_LAYOUTS: &[SlotLayoutField] = &[
    SlotLayoutField {
        name: "arg_layout",
        count_bits: 32,
        slot_type_bits: 8,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
    SlotLayoutField {
        name: "ret_layout",
        count_bits: 32,
        slot_type_bits: 8,
        authority: LayoutAuthority::CodegenTypeInfo,
    },
];
pub(super) const FIELD_META_QUEUE_LAYOUTS: &[SlotLayoutField] = &[SlotLayoutField {
    name: "elem_layout",
    count_bits: 32,
    slot_type_bits: 8,
    authority: LayoutAuthority::CodegenTypeInfo,
}];
pub(super) const FIELD_META_MAP_ITER_NEXT_LAYOUTS: &[SlotLayoutField] = FIELD_META_MAP_GET_LAYOUTS;
pub(super) const FIELD_META_IFACE_ASSERT_LAYOUTS: &[SlotLayoutField] = &[SlotLayoutField {
    name: "result_layout",
    count_bits: 32,
    slot_type_bits: 8,
    authority: LayoutAuthority::CodegenTypeInfo,
}];
pub(super) const FIELD_META_LOOP_END: &[FieldWidth] = &[FieldWidth {
    name: "end_pc",
    bits: 32,
    max: None,
    domain: FieldDomain::Any,
    authority: LayoutAuthority::CodegenTypeInfo,
}];
