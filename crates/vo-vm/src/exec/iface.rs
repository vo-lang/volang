//! Interface instructions: IfaceAssign, IfaceAssert
//!
//! slot0 format: [itab_id:32 | rttid:24 | value_kind:8]
//! slot1: data (immediate value or GcRef)

use vo_common_core::types::ValueKind;
use vo_runtime_core::gc::{Gc, GcRef};
use vo_runtime_core::objects::interface;

use crate::bytecode::{Constant, Module};
use crate::fiber::Fiber;
use crate::instruction::Instruction;
use crate::itab::ItabCache;
use crate::vm::ExecResult;

/// IfaceAssign: a=dst (2 slots), b=src, c=const_idx, flags=value_kind
///
/// c points to Int64 constant:
/// - Concrete type: (rttid << 32) | itab_id, itab built at compile time
/// - Interface source: iface_meta_id (high 32 bits = 0), itab built at runtime
pub fn exec_iface_assign(
    fiber: &mut Fiber,
    inst: &Instruction,
    gc: &mut Gc,
    itab_cache: &mut ItabCache,
    module: &Module,
) {
    let vk = ValueKind::from_u8(inst.flags);
    let src = fiber.read_reg(inst.b);

    // Unpack metadata from constant pool
    let packed = match &module.constants[inst.c as usize] {
        Constant::Int(v) => *v,
        _ => panic!("IfaceAssign: expected Int64 constant"),
    };
    let rttid = (packed >> 32) as u32;
    let low = (packed & 0xFFFFFFFF) as u32;

    let (actual_rttid, actual_vk, itab_id) = if vk == ValueKind::Interface {
        // Interface -> Interface: runtime itab lookup/creation
        let src_slot0 = src;
        let src_rttid = interface::unpack_rttid(src_slot0);
        let src_vk = interface::unpack_value_kind(src_slot0);
        let iface_meta_id = low;  // low = target iface_meta_id
        
        let itab_id = if src_rttid == 0 {
            0  // primitive or nil
        } else {
            // Get named_type_id from runtime_types (rttid != named_type_id)
            let named_type_id = if let Some(vo_common_core::RuntimeType::Named(id)) = module.runtime_types.get(src_rttid as usize) {
                *id
            } else {
                0 // non-Named types don't have methods
            };
            itab_cache.get_or_create(
                named_type_id,
                iface_meta_id,
                &module.named_type_metas,
                &module.interface_metas,
            )
        };
        (src_rttid, src_vk, itab_id)
    } else {
        // Concrete type -> Interface: compile-time itab
        (rttid, vk, low)  // low = itab_id
    };

    let slot0 = interface::pack_slot0(itab_id, actual_rttid, actual_vk);

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
/// flags = assert_kind | (has_ok << 2) | (target_slots << 3)
/// assert_kind: 0=rttid comparison, 1=interface method check
/// For struct/array (determined by src_vk), copies value from GcRef to dst registers
/// For interface (assert_kind=1), returns new interface with itab for target interface
pub fn exec_iface_assert(
    fiber: &mut Fiber,
    inst: &Instruction,
    itab_cache: &mut ItabCache,
    module: &Module,
) -> ExecResult {
    let slot0 = fiber.read_reg(inst.b);
    let slot1 = fiber.read_reg(inst.b + 1);

    let assert_kind = inst.flags & 0x3;
    let has_ok = ((inst.flags >> 2) & 0x1) != 0;
    let target_slots = (inst.flags >> 3) as u16;
    let target_id = inst.c as u32;

    let src_rttid = interface::unpack_rttid(slot0);
    let src_vk = interface::unpack_value_kind(slot0);

    let matches = match assert_kind {
        0 => {
            // Type comparison: check rttid
            src_rttid == target_id
        }
        1 => {
            // Interface: check if src type satisfies target interface
            let iface_meta = &module.interface_metas[target_id as usize];
            if iface_meta.methods.is_empty() {
                true // empty interface always satisfied
            } else {
                // Look up RuntimeType to find named_type_id for method lookup
                if let Some(rt) = module.runtime_types.get(src_rttid as usize) {
                    if let vo_common_core::RuntimeType::Named(named_type_id) = rt {
                        let named_type = &module.named_type_metas[*named_type_id as usize];
                        // Check each interface method: name must exist AND signature must match
                        iface_meta.methods.iter().all(|iface_method| {
                            if let Some(concrete_method) = named_type.methods.get(&iface_method.name) {
                                // Compare signatures using InterfaceMethod::matches_signature
                                let iface_method_wrapper = vo_common_core::InterfaceMethod::new(
                                    vo_common_core::symbol::Symbol::DUMMY,
                                    iface_method.signature.clone(),
                                );
                                iface_method_wrapper.matches_signature(&concrete_method.signature)
                            } else {
                                false // method not found
                            }
                        })
                    } else {
                        false // non-named types can't implement interfaces with methods
                    }
                } else {
                    false
                }
            }
        }
        _ => false,
    };

    // Helper to write successful result
    let write_success = |fiber: &mut Fiber, itab_cache: &mut ItabCache| {
        if assert_kind == 1 {
            // Interface assertion: return new interface with itab for target interface
            // Need to get named_type_id from runtime_types (rttid != named_type_id)
            let named_type_id = if let Some(vo_common_core::RuntimeType::Named(id)) = module.runtime_types.get(src_rttid as usize) {
                *id
            } else {
                0 // Should not happen for valid interface assertion
            };
            let new_itab_id = itab_cache.get_or_create(
                named_type_id,
                target_id,
                &module.named_type_metas,
                &module.interface_metas,
            );
            let new_slot0 = interface::pack_slot0(new_itab_id, src_rttid, src_vk);
            fiber.write_reg(inst.a, new_slot0);
            fiber.write_reg(inst.a + 1, slot1);
        } else if src_vk == ValueKind::Struct || src_vk == ValueKind::Array {
            // Concrete type assertion for struct/array: copy value from GcRef
            let gc_ref = slot1 as GcRef;
            let slots = target_slots.max(1);
            if slot1 != 0 {
                for i in 0..slots {
                    let val = unsafe { *gc_ref.add(i as usize) };
                    fiber.write_reg(inst.a + i, val);
                }
            } else {
                for i in 0..slots {
                    fiber.write_reg(inst.a + i, 0);
                }
            }
        } else {
            // Concrete type assertion for other types: slot1 is the value
            fiber.write_reg(inst.a, slot1);
        }
    };

    if has_ok {
        let ok_slot = if assert_kind == 1 { inst.a + 2 } else if target_slots > 1 { inst.a + target_slots } else { inst.a + 1 };
        fiber.write_reg(ok_slot, matches as u64);
        if matches {
            write_success(fiber, itab_cache);
        } else {
            // Zero out destination on failure
            let dst_slots = if assert_kind == 1 { 2 } else { target_slots.max(1) };
            for i in 0..dst_slots {
                fiber.write_reg(inst.a + i, 0);
            }
        }
        ExecResult::Continue
    } else if matches {
        write_success(fiber, itab_cache);
        ExecResult::Continue
    } else {
        ExecResult::Panic
    }
}
