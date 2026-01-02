//! Interface instructions: IfaceAssign, IfaceAssert
//!
//! slot0 format: [itab_id:32 | rttid:24 | value_kind:8]
//! slot1: data (immediate value or GcRef)

use vo_runtime::{RuntimeType, ValueKind};
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::interface;

use crate::bytecode::{Constant, Module};
use crate::instruction::Instruction;
use crate::itab::ItabCache;
use crate::vm::ExecResult;

/// Extract named_type_id from RuntimeType (recursively unwraps Pointer).
/// Methods are always defined on the base Named type.
fn extract_named_type_id(rt: &RuntimeType) -> Option<u32> {
    match rt {
        RuntimeType::Named(id) => Some(*id),
        RuntimeType::Pointer(inner) => extract_named_type_id(inner),
        _ => None,
    }
}

/// IfaceAssign: a=dst (2 slots), b=src, c=const_idx, flags=value_kind
///
/// c points to Int64 constant:
/// - Concrete type: (rttid << 32) | itab_id, itab built at compile time
/// - Interface source: iface_meta_id (high 32 bits = 0), itab built at runtime
pub fn exec_iface_assign(
    stack: &mut [u64],
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    itab_cache: &mut ItabCache,
    module: &Module,
) {
    let vk = ValueKind::from_u8(inst.flags);
    let src = stack[bp + inst.b as usize];

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
        
        let itab_id = {
            // Get named_type_id from runtime_types
            // For Named types: Named(id) -> id
            // For Pointer types: Pointer(Named(id)) -> id (methods are on base type)
            let named_type_id = module.runtime_types.get(src_rttid as usize)
                .and_then(|rt| extract_named_type_id(rt))
                .unwrap_or(0);
            
            if named_type_id == 0 {
                0  // primitive or nil - no methods
            } else {
                itab_cache.get_or_create(
                    named_type_id,
                    iface_meta_id,
                    &module.named_type_metas,
                    &module.interface_metas,
                )
            }
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
            let src_slot1 = stack[bp + inst.b as usize + 1];
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

    stack[bp + inst.a as usize] = slot0;
    stack[bp + inst.a as usize + 1] = slot1;
}

/// IfaceAssert: a=dst, b=src_iface (2 slots), c=target_id
/// flags = assert_kind | (has_ok << 2) | (target_slots << 3)
/// assert_kind: 0=rttid comparison, 1=interface method check
/// For struct/array (determined by src_vk), copies value from GcRef to dst registers
/// For interface (assert_kind=1), returns new interface with itab for target interface
pub fn exec_iface_assert(
    stack: &mut [u64],
    bp: usize,
    inst: &Instruction,
    itab_cache: &mut ItabCache,
    module: &Module,
) -> ExecResult {
    let slot0 = stack[bp + inst.b as usize];
    let slot1 = stack[bp + inst.b as usize + 1];

    let assert_kind = inst.flags & 0x3;
    let has_ok = ((inst.flags >> 2) & 0x1) != 0;
    let target_slots = (inst.flags >> 3) as u16;
    let target_id = inst.c as u32;

    let src_rttid = interface::unpack_rttid(slot0);
    let src_vk = interface::unpack_value_kind(slot0);

    // nil interface (value_kind == Void) always fails assertion
    let matches = if src_vk == ValueKind::Void {
        false
    } else {
        match assert_kind {
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
                    // Use extract_named_type_id to handle Pointer(Named(id)) case
                    if let Some(named_type_id) = module.runtime_types.get(src_rttid as usize)
                        .and_then(|rt| extract_named_type_id(rt))
                    {
                        let named_type = &module.named_type_metas[named_type_id as usize];
                        // Check each interface method: name must exist AND signature must match
                        iface_meta.methods.iter().all(|iface_method| {
                            if let Some(concrete_method) = named_type.methods.get(&iface_method.name) {
                                // Compare signatures using InterfaceMethod::matches_signature
                                let iface_method_wrapper = vo_runtime::InterfaceMethod::new(
                                    vo_runtime::symbol::Symbol::DUMMY,
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
                }
            }
            _ => false,
        }
    };

    // Helper to write successful result
    let write_success = |stack: &mut [u64], itab_cache: &mut ItabCache| {
        if assert_kind == 1 {
            // Interface assertion: return new interface with itab for target interface
            // Use extract_named_type_id to handle Pointer(Named(id)) case
            let named_type_id = module.runtime_types.get(src_rttid as usize)
                .and_then(|rt| extract_named_type_id(rt))
                .unwrap_or(0);
            let new_itab_id = itab_cache.get_or_create(
                named_type_id,
                target_id,
                &module.named_type_metas,
                &module.interface_metas,
            );
            let new_slot0 = interface::pack_slot0(new_itab_id, src_rttid, src_vk);
            stack[bp + inst.a as usize] = new_slot0;
            stack[bp + inst.a as usize + 1] = slot1;
        } else if src_vk == ValueKind::Struct || src_vk == ValueKind::Array {
            // Concrete type assertion for struct/array: copy value from GcRef
            let gc_ref = slot1 as GcRef;
            let slots = target_slots.max(1);
            if slot1 != 0 {
                for i in 0..slots {
                    let val = unsafe { *gc_ref.add(i as usize) };
                    stack[bp + inst.a as usize + i as usize] = val;
                }
            } else {
                for i in 0..slots {
                    stack[bp + inst.a as usize + i as usize] = 0;
                }
            }
        } else {
            // Concrete type assertion for other types: slot1 is the value
            stack[bp + inst.a as usize] = slot1;
        }
    };

    if has_ok {
        let ok_slot = if assert_kind == 1 { inst.a + 2 } else if target_slots > 1 { inst.a + target_slots } else { inst.a + 1 };
        stack[bp + ok_slot as usize] = matches as u64;
        if matches {
            write_success(stack, itab_cache);
        } else {
            // Zero out destination on failure
            let dst_slots = if assert_kind == 1 { 2 } else { target_slots.max(1) };
            for i in 0..dst_slots {
                stack[bp + inst.a as usize + i as usize] = 0;
            }
        }
        ExecResult::Continue
    } else if matches {
        write_success(stack, itab_cache);
        ExecResult::Continue
    } else {
        ExecResult::Panic
    }
}
