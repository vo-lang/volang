//! Interface instructions: IfaceAssign, IfaceAssert

use vo_common_core::types::{ValueKind, ValueMeta};
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::interface;

use crate::bytecode::Module;
use crate::fiber::Fiber;
use crate::instruction::Instruction;
use crate::itab::ItabCache;
use crate::vm::ExecResult;

pub fn exec_iface_assign(
    fiber: &mut Fiber,
    inst: &Instruction,
    gc: &mut Gc,
    itab_cache: &mut ItabCache,
    module: &Module,
) {
    let iface_meta_id = inst.c as u32;
    let vk = ValueKind::from_u8(inst.flags);

    let src = fiber.read_reg(inst.b);

    let src_meta_id = match vk {
        ValueKind::Struct | ValueKind::Array | ValueKind::Pointer => {
            if src != 0 {
                Gc::header(src as GcRef).meta_id()
            } else {
                0
            }
        }
        ValueKind::Interface => {
            interface::unpack_value_meta(src).meta_id()
        }
        _ => 0,
    };

    let itab_id = itab_cache.get_or_create(
        src_meta_id,
        iface_meta_id,
        &module.struct_metas,
        &module.interface_metas,
    );

    let value_meta = ValueMeta::new(src_meta_id, vk);
    let slot0 = interface::pack_slot0(itab_id, value_meta);

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

pub fn exec_iface_assert(fiber: &mut Fiber, inst: &Instruction) -> ExecResult {
    let slot0 = fiber.read_reg(inst.b);
    let slot1 = fiber.read_reg(inst.b + 1);

    let value_meta = interface::unpack_value_meta(slot0);
    let target_meta_id = inst.c as u32;
    let has_ok = inst.flags > 0;

    let matches = value_meta.meta_id() == target_meta_id;

    if has_ok {
        let ok_reg = inst.a + inst.flags as u16;
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
