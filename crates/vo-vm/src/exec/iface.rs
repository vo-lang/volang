//! Interface instructions: IfaceAssign, IfaceAssert
//!
//! slot0 format: [itab_id:32 | named_type_id:24 | value_kind:8]
//! slot1: data (immediate value or GcRef)

use vo_common_core::types::ValueKind;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::interface;

use crate::bytecode::Module;
use crate::fiber::Fiber;
use crate::instruction::Instruction;
use crate::itab::ItabCache;
use crate::vm::ExecResult;

/// IfaceAssign: a=dst (2 slots), b=src, c=iface_meta_id, flags=value_kind
/// For now, named_type_id is embedded in the high bits of b (b = src_slot | (named_type_id << 8))
/// Or use a preceding LoadInt to set up named_type_id
pub fn exec_iface_assign(
    fiber: &mut Fiber,
    inst: &Instruction,
    gc: &mut Gc,
    itab_cache: &mut ItabCache,
    module: &Module,
) {
    let iface_meta_id = inst.c as u32;
    let vk = ValueKind::from_u8(inst.flags);
    // For now, named_type_id is 0 for non-interface sources (TODO: proper encoding)
    let named_type_id = 0u32;

    let src = fiber.read_reg(inst.b);

    let iface_meta = &module.interface_metas[iface_meta_id as usize];
    let empty_iface = iface_meta.method_names.is_empty();

    // For interface source, extract the original named_type_id and value_kind
    let (actual_named_type_id, actual_vk) = match vk {
        ValueKind::Interface => {
            let src_named_type_id = interface::unpack_named_type_id(src);
            let src_vk = interface::unpack_value_kind(src);
            (src_named_type_id, src_vk)
        }
        _ => (named_type_id, vk),
    };

    let itab_id = if empty_iface || actual_named_type_id == 0 {
        0
    } else {
        itab_cache.get_or_create(
            actual_named_type_id,
            iface_meta_id,
            &module.named_type_metas,
            &module.interface_metas,
        )
    };

    let slot0 = interface::pack_slot0(itab_id, actual_named_type_id, actual_vk);

    let slot1 = match vk {
        ValueKind::Struct | ValueKind::Array => {
            if src != 0 {
                unsafe { gc.ptr_clone(src as GcRef) as u64 }
            } else {
                0
            }
        }
        ValueKind::Interface => {
            let src_slot0 = src;
            let src_slot1 = fiber.read_reg(inst.b + 1);
            let src_vk = interface::unpack_value_kind(src_slot0);
            if src_vk == ValueKind::Struct || src_vk == ValueKind::Array {
                if src_slot1 != 0 {
                    unsafe { gc.ptr_clone(src_slot1 as GcRef) as u64 }
                } else {
                    0
                }
            } else {
                src_slot1
            }
        }
        _ => src,
    };

    fiber.write_reg(inst.a, slot0);
    fiber.write_reg(inst.a + 1, slot1);
}

/// IfaceAssert: a=dst, b=src_iface (2 slots), c=target_id
/// flags = assert_kind | (has_ok << 2)
/// assert_kind: 0=Primitive, 1=Named, 2=Interface
pub fn exec_iface_assert(
    fiber: &mut Fiber,
    inst: &Instruction,
    itab_cache: &mut ItabCache,
    module: &Module,
) -> ExecResult {
    let slot0 = fiber.read_reg(inst.b);
    let slot1 = fiber.read_reg(inst.b + 1);

    let assert_kind = inst.flags & 0x3;
    let has_ok = (inst.flags >> 2) != 0;
    let target_id = inst.c as u32;

    let src_named_type_id = interface::unpack_named_type_id(slot0);
    let src_vk = interface::unpack_value_kind(slot0);

    let matches = match assert_kind {
        0 => {
            // Primitive: check value_kind, named_type_id must be 0
            src_named_type_id == 0 && src_vk as u32 == target_id
        }
        1 => {
            // Named: check named_type_id
            src_named_type_id == target_id
        }
        2 => {
            // Interface: check if src's named type satisfies target interface
            if src_named_type_id == 0 {
                false
            } else {
                // Try to build itab - if succeeds, type satisfies interface
                let iface_meta = &module.interface_metas[target_id as usize];
                if iface_meta.method_names.is_empty() {
                    true // empty interface always satisfied
                } else {
                    let named_type = &module.named_type_metas[src_named_type_id as usize];
                    iface_meta.method_names.iter().all(|name| named_type.methods.contains_key(name))
                }
            }
        }
        _ => false,
    };

    if has_ok {
        let ok_reg = inst.a + 1; // ok follows dst
        fiber.write_reg(ok_reg, matches as u64);
        if matches {
            fiber.write_reg(inst.a, slot1);
        } else {
            fiber.write_reg(inst.a, 0);
        }
        ExecResult::Continue
    } else if matches {
        fiber.write_reg(inst.a, slot1);
        ExecResult::Continue
    } else {
        ExecResult::Panic
    }
}
