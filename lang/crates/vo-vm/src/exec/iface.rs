//! Interface instructions: IfaceAssign, IfaceAssert
//!
//! slot0 format: [itab_id:32 | rttid:24 | value_kind:8]
//! slot1: data (immediate value or GcRef)

use vo_runtime::{RuntimeType, ValueKind};
use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::objects::interface;
use vo_runtime::itab::{self, ItabCache};
use vo_runtime::slot::Slot;

use crate::bytecode::{Constant, Module};
use crate::instruction::Instruction;
use crate::vm::ExecResult;
use crate::vm::helpers::{stack_get, stack_set};

/// Extract named_type_id from RuntimeType (recursively unwraps Pointer).
/// Methods are always defined on the base Named type.
fn extract_named_type_id(rt: &RuntimeType, runtime_types: &[RuntimeType]) -> Option<u32> {
    match rt {
        RuntimeType::Named { id, .. } => Some(*id),
        RuntimeType::Pointer(elem_value_rttid) => {
            runtime_types.get(elem_value_rttid.rttid() as usize)
                .and_then(|inner| extract_named_type_id(inner, runtime_types))
        }
        _ => None,
    }
}

/// IfaceAssign: a=dst (2 slots), b=src, c=const_idx, flags=value_kind
///
/// c points to Int64 constant:
/// - Concrete type: (rttid << 32) | itab_id, itab built at compile time
/// - Interface source: iface_meta_id (high 32 bits = 0), itab built at runtime
pub fn exec_iface_assign(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    gc: &mut Gc,
    itab_cache: &mut ItabCache,
    module: &Module,
) {
    let vk = ValueKind::from_u8(inst.flags);
    let src = stack_get(stack, bp + inst.b as usize);

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
        
        let itab_id = if iface_meta_id == 0 {
            // Target is any (empty interface): itab_id must be 0
            // This invariant is relied upon by codegen optimizations
            0
        } else {
            // Get named_type_id from runtime_types
            // For Named types: Named(id) -> id
            // For Pointer types: Pointer(Named(id)) -> id (methods are on base type)
            // Note: named_type_id=0 is valid (e.g., bytes.Buffer), so use Option
            let named_type_id_opt = module.runtime_types.get(src_rttid as usize)
                .and_then(|rt| extract_named_type_id(rt, &module.runtime_types));
            
            if let Some(named_type_id) = named_type_id_opt {
                // Value types (non-pointer) cannot use pointer receiver methods
                let src_is_pointer = src_vk == ValueKind::Pointer;
                itab_cache.get_or_create(
                    named_type_id,
                    iface_meta_id,
                    src_is_pointer,
                    &module.named_type_metas,
                    &module.interface_metas,
                )
            } else {
                0  // primitive or nil - no methods, itab_id=0 means empty itab
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
            let src_slot1 = stack_get(stack, bp + inst.b as usize + 1);
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

    stack_set(stack, bp + inst.a as usize, slot0);
    stack_set(stack, bp + inst.a as usize + 1, slot1);
}

/// IfaceAssert: a=dst, b=src_iface (2 slots), c=target_id
/// flags = assert_kind | (has_ok << 2) | (target_slots << 3)
/// assert_kind: 0=rttid comparison, 1=interface method check
/// For struct/array (determined by src_vk), copies value from GcRef to dst registers
/// For interface (assert_kind=1), returns new interface with itab for target interface
pub fn exec_iface_assert(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    itab_cache: &mut ItabCache,
    module: &Module,
) -> ExecResult {
    let slot0 = stack_get(stack, bp + inst.b as usize);
    let slot1 = stack_get(stack, bp + inst.b as usize + 1);

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
                itab::check_interface_satisfaction(src_rttid, src_vk, target_id, module)
            }
            _ => false,
        }
    };

    // Helper to write successful result
    let write_success = |stack: *mut Slot, itab_cache: &mut ItabCache| {
        if assert_kind == 1 {
            // Interface assertion: return new interface with itab for target interface
            // Use extract_named_type_id to handle Pointer(Named(id)) case
            let named_type_id = module.runtime_types.get(src_rttid as usize)
                .and_then(|rt| extract_named_type_id(rt, &module.runtime_types))
                .unwrap_or(0);
            // Value types (non-pointer) cannot use pointer receiver methods
            let src_is_pointer = src_vk == ValueKind::Pointer;
            let new_itab_id = itab_cache.get_or_create(
                named_type_id,
                target_id,
                src_is_pointer,
                &module.named_type_metas,
                &module.interface_metas,
            );
            let new_slot0 = interface::pack_slot0(new_itab_id, src_rttid, src_vk);
            stack_set(stack, bp + inst.a as usize, new_slot0);
            stack_set(stack, bp + inst.a as usize + 1, slot1);
        } else if src_vk == ValueKind::Struct {
            // Concrete type assertion for struct: copy value from GcRef
            let gc_ref = slot1 as GcRef;
            let slots = target_slots.max(1);
            if slot1 != 0 {
                for i in 0..slots {
                    let val = unsafe { *gc_ref.add(i as usize) };
                    stack_set(stack, bp + inst.a as usize + i as usize, val);
                }
            } else {
                for i in 0..slots {
                    stack_set(stack, bp + inst.a as usize + i as usize, 0);
                }
            }
        } else if src_vk == ValueKind::Array {
            // Concrete type assertion for array: copy elements from GcRef
            // Array layout: [ArrayHeader(2 slots)][elements...]
            use vo_runtime::objects::array;
            let gc_ref = slot1 as GcRef;
            let slots = target_slots.max(1);
            if slot1 != 0 {
                // Copy data from after ArrayHeader (data_ptr_bytes skips ArrayHeader)
                let data_ptr = array::data_ptr_bytes(gc_ref) as *const u64;
                for i in 0..slots {
                    let val = unsafe { *data_ptr.add(i as usize) };
                    stack_set(stack, bp + inst.a as usize + i as usize, val);
                }
            } else {
                for i in 0..slots {
                    stack_set(stack, bp + inst.a as usize + i as usize, 0);
                }
            }
        } else {
            // Concrete type assertion for other types: slot1 is the value
            stack_set(stack, bp + inst.a as usize, slot1);
        }
    };

    if has_ok {
        let ok_slot = if assert_kind == 1 { inst.a + 2 } else if target_slots > 1 { inst.a + target_slots } else { inst.a + 1 };
        stack_set(stack, bp + ok_slot as usize, matches as u64);
        if matches {
            write_success(stack, itab_cache);
        } else {
            // Zero out destination on failure
            let dst_slots = if assert_kind == 1 { 2 } else { target_slots.max(1) };
            for i in 0..dst_slots {
                stack_set(stack, bp + inst.a as usize + i as usize, 0);
            }
        }
        ExecResult::FrameChanged
    } else if matches {
        write_success(stack, itab_cache);
        ExecResult::FrameChanged
    } else {
        ExecResult::Panic
    }
}

/// IfaceEq: a = (b == c) where b,c are 2-slot interface values
/// Go interface equality: same dynamic type (rttid + vk) AND same dynamic value
/// Panics if dynamic type is not comparable (slice, map, func, chan).
pub fn exec_iface_eq(stack: *mut Slot, bp: usize, inst: &Instruction, module: &Module) -> ExecResult {
    use vo_runtime::objects::compare;
    
    let b0 = stack_get(stack, bp + inst.b as usize);
    let b1 = stack_get(stack, bp + inst.b as usize + 1);
    let c0 = stack_get(stack, bp + inst.c as usize);
    let c1 = stack_get(stack, bp + inst.c as usize + 1);
    
    let result = compare::iface_eq(b0, b1, c0, c1, module);
    
    if result == 2 {
        return ExecResult::Panic;
    }
    
    stack_set(stack, bp + inst.a as usize, result);
    ExecResult::FrameChanged
}
