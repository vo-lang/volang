//! Interface instructions: IfaceAssign, IfaceAssert
//!
//! slot0 format: [itab_id:32 | rttid:24 | value_kind:8]
//! slot1: data (immediate value or GcRef)

#[cfg(not(feature = "std"))]
use alloc::{
    format,
    string::{String, ToString},
    vec,
};
#[cfg(feature = "std")]
use std::{string::String, vec};

use vo_runtime::gc::{Gc, GcRef};
use vo_runtime::itab::{self, ItabCache};
use vo_runtime::objects::interface;
use vo_runtime::slot::Slot;
#[cfg(test)]
use vo_runtime::RuntimeType;
use vo_runtime::ValueKind;

use crate::bytecode::{Constant, Module, IFACE_ASSIGN_NO_ITAB};
use crate::instruction::{Instruction, IFACE_ASSERT_MAX_TARGET_SLOTS};
use crate::vm::helpers::{stack_get, stack_set};
use crate::vm::ExecResult;

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
) -> Result<(), String> {
    let vk = ValueKind::try_from(inst.flags)
        .map_err(|_| format!("IfaceAssign invalid ValueKind tag {}", inst.flags))?;
    let src = stack_get(stack, bp + inst.b as usize);

    // Unpack metadata from constant pool
    let constant = module.constants.get(inst.c as usize).ok_or_else(|| {
        format!(
            "IfaceAssign constant index {} out of bounds for {} constants",
            inst.c,
            module.constants.len()
        )
    })?;
    let packed = match constant.clone() {
        Constant::Int(v) => v,
        _ => {
            return Err(format!(
                "IfaceAssign constant index {} must be Int metadata",
                inst.c
            ));
        }
    };
    let rttid = (packed >> 32) as u32;
    let low = (packed & 0xFFFFFFFF) as u32;

    let (actual_rttid, actual_vk, itab_id) = if vk == ValueKind::Interface {
        // Interface -> Interface: runtime itab lookup/creation
        let src_slot0 = src;
        if src_slot0 == 0 {
            stack_set(stack, bp + inst.a as usize, 0);
            stack_set(stack, bp + inst.a as usize + 1, 0);
            return Ok(());
        }
        let src_rttid = interface::unpack_rttid(src_slot0);
        let src_vk = interface::unpack_value_kind(src_slot0);
        let iface_meta_id = low; // low = target iface_meta_id

        let itab_id = if iface_meta_id == 0 {
            // Target is any (empty interface): itab_id must be 0
            // This invariant is relied upon by codegen optimizations
            0
        } else {
            // Get named_type_id from runtime_types
            // For Named types: Named(id) -> id
            // For Pointer types: Pointer(Named(id)) -> id (methods are on base type)
            // Note: named_type_id=0 is valid (e.g., bytes.Buffer), so use Option
            let named_type_id_opt = module.named_type_id_for_rttid(src_rttid);

            if let Some(named_type_id) = named_type_id_opt {
                // Value types (non-pointer) cannot use pointer receiver methods
                let src_is_pointer = src_vk == ValueKind::Pointer;
                itab_cache
                    .try_get_or_create(
                        named_type_id,
                        iface_meta_id,
                        src_is_pointer,
                        &module.named_type_metas,
                        &module.interface_metas,
                    )
                    .ok_or_else(|| {
                        format!(
                            "IfaceAssign interface conversion failed: named_type_id={named_type_id} target_iface_meta_id={iface_meta_id} src_vk={src_vk:?}"
                        )
                    })?
            } else if iface_meta_id == 0 {
                0
            } else {
                return Err(format!(
                    "IfaceAssign interface conversion missing named receiver: target_iface_meta_id={iface_meta_id} src_rttid={src_rttid} src_vk={src_vk:?}"
                ));
            }
        };
        (src_rttid, src_vk, itab_id)
    } else {
        // Concrete type -> Interface: compile-time itab
        let itab_id = if low == IFACE_ASSIGN_NO_ITAB { 0 } else { low };
        (rttid, vk, itab_id)
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
    Ok(())
}

/// IfaceAssert: a=dst, b=src_iface (2 slots), c=target-id mirror/sentinel
/// flags = assert-kind mirror | (has_ok << 2) | (result-slot mirror << 3)
/// Full target identity and result layout come from `IfaceAssertLayout` metadata.
/// assert_kind: 0=rttid comparison, 1=interface method check
/// For struct/array (determined by src_vk), copies value from GcRef to dst registers
/// For interface (assert_kind=1), returns new interface with itab for target interface
pub fn exec_iface_assert(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    metadata_assert_kind: u8,
    target_id: u32,
    result_layout: &[vo_runtime::SlotType],
    itab_cache: &mut ItabCache,
    module: &Module,
) -> ExecResult {
    let slot0 = stack_get(stack, bp + inst.b as usize);
    let slot1 = stack_get(stack, bp + inst.b as usize + 1);

    let assert_kind = metadata_assert_kind;
    let has_ok = ((inst.flags >> 2) & 0x1) != 0;
    if inst.flags & 0x3 != assert_kind {
        return ExecResult::JitError(format!(
            "IfaceAssert kind mirror {} does not match metadata kind {assert_kind}",
            inst.flags & 0x3
        ));
    }
    let Ok(logical_result_slots) = u16::try_from(result_layout.len()) else {
        return ExecResult::JitError("IfaceAssert result layout exceeds u16 slots".to_string());
    };
    let expected_slot_mirror = if logical_result_slots <= IFACE_ASSERT_MAX_TARGET_SLOTS {
        logical_result_slots
    } else {
        0
    };
    if u16::from(inst.flags >> 3) != expected_slot_mirror {
        return ExecResult::JitError(format!(
            "IfaceAssert result-slot mirror {} does not match metadata width {logical_result_slots}",
            inst.flags >> 3
        ));
    }
    let expected_target_mirror = u16::try_from(target_id).unwrap_or(u16::MAX);
    if inst.c != expected_target_mirror {
        return ExecResult::JitError(format!(
            "IfaceAssert target mirror {} does not match metadata target {target_id}",
            inst.c
        ));
    }

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

    let materialize_success = |out: &mut [Slot],
                               itab_cache: &mut ItabCache|
     -> Result<usize, String> {
        if assert_kind == 1 {
            // Interface assertion: return new interface with itab for target interface
            let new_itab_id = if target_id == 0 {
                0
            } else {
                let Some(named_type_id) = module.named_type_id_for_rttid(src_rttid) else {
                    return Err(format!(
                        "IfaceAssert metadata missing: target_id={} src_rttid={} src_vk={:?}",
                        target_id, src_rttid, src_vk
                    ));
                };
                // Value types (non-pointer) cannot use pointer receiver methods
                let src_is_pointer = src_vk == ValueKind::Pointer;
                let Some(itab_id) = itab_cache.try_get_or_create(
                    named_type_id,
                    target_id,
                    src_is_pointer,
                    &module.named_type_metas,
                    &module.interface_metas,
                ) else {
                    return Err(format!(
                        "IfaceAssert itab metadata missing: target_id={} named_type_id={} src_vk={:?}",
                        target_id, named_type_id, src_vk
                    ));
                };
                itab_id
            };
            out[0] = interface::pack_slot0(new_itab_id, src_rttid, src_vk);
            out[1] = slot1;
            Ok(2)
        } else if src_vk == ValueKind::Struct {
            // Concrete type assertion for struct: copy value from GcRef
            let gc_ref = slot1 as GcRef;
            let slots = logical_result_slots;
            if slot1 != 0 {
                for i in 0..slots {
                    out[i as usize] = unsafe { *gc_ref.add(i as usize) };
                }
            } else {
                for i in 0..slots {
                    out[i as usize] = 0;
                }
            }
            Ok(slots as usize)
        } else if src_vk == ValueKind::Array {
            use vo_runtime::objects::array;
            let gc_ref = slot1 as GcRef;
            unsafe { array::read_value_flat(gc_ref, out) }.map_err(String::from)?;
            Ok(out.len())
        } else {
            // Concrete type assertion for other types: slot1 is the value
            if logical_result_slots == 0 {
                Ok(0)
            } else {
                out[0] = slot1;
                Ok(1)
            }
        }
    };

    if has_ok {
        let Some(ok_slot) = inst.a.checked_add(logical_result_slots) else {
            return ExecResult::JitError("IfaceAssert destination slot overflow".to_string());
        };
        if matches {
            let mut result_slots = vec![0; logical_result_slots as usize];
            let result_len = match materialize_success(&mut result_slots, itab_cache) {
                Ok(result_len) => result_len,
                Err(msg) => return ExecResult::JitError(msg),
            };
            for (idx, value) in result_slots.iter().take(result_len).enumerate() {
                stack_set(stack, bp + inst.a as usize + idx, *value);
            }
            stack_set(stack, bp + ok_slot as usize, 1);
        } else {
            // Zero out destination on failure
            for i in 0..logical_result_slots {
                stack_set(stack, bp + inst.a as usize + i as usize, 0);
            }
            stack_set(stack, bp + ok_slot as usize, 0);
        }
        ExecResult::FrameChanged
    } else if matches {
        let mut result_slots = vec![0; logical_result_slots as usize];
        let result_len = match materialize_success(&mut result_slots, itab_cache) {
            Ok(result_len) => result_len,
            Err(msg) => return ExecResult::JitError(msg),
        };
        for (idx, value) in result_slots.iter().take(result_len).enumerate() {
            stack_set(stack, bp + inst.a as usize + idx, *value);
        }
        ExecResult::FrameChanged
    } else {
        ExecResult::Panic
    }
}

/// IfaceEq: a = (b == c) where b,c are 2-slot interface values
/// Go interface equality: same dynamic type (rttid + vk) AND same dynamic value
/// Panics if dynamic type is not comparable (slice, map, func, chan).
pub fn exec_iface_eq(
    stack: *mut Slot,
    bp: usize,
    inst: &Instruction,
    module: &Module,
) -> ExecResult {
    use vo_runtime::objects::compare;

    let b0 = stack_get(stack, bp + inst.b as usize);
    let b1 = stack_get(stack, bp + inst.b as usize + 1);
    let c0 = stack_get(stack, bp + inst.c as usize);
    let c1 = stack_get(stack, bp + inst.c as usize + 1);

    // Safety: verified interface operands stay rooted for this instruction.
    let result = unsafe { compare::iface_eq(b0, b1, c0, c1, module) };

    if result == 2 {
        return ExecResult::Panic;
    }

    stack_set(stack, bp + inst.a as usize, result);
    ExecResult::FrameChanged
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::InterfaceMeta;
    use crate::instruction::{pack_iface_assert_flags, Opcode};

    #[test]
    fn exec_iface_assert_has_ok_does_not_write_ok_before_success_materialization_061() {
        let mut module = Module::new("iface-assert-commit-order".to_string());
        module
            .runtime_types
            .push(RuntimeType::Basic(ValueKind::String));
        module.interface_metas.push(InterfaceMeta {
            name: "Any0".to_string(),
            method_names: Vec::new(),
            methods: Vec::new(),
        });
        module.interface_metas.push(InterfaceMeta {
            name: "Any1".to_string(),
            method_names: Vec::new(),
            methods: Vec::new(),
        });
        let flags = pack_iface_assert_flags(1, true, 2).expect("valid IfaceAssert flags");
        let inst = Instruction::with_flags(Opcode::IfaceAssert, flags, 2, 0, 1);
        let mut itab_cache = ItabCache::new();
        let source_slot0 = interface::pack_slot0(0, 0, ValueKind::String);
        let mut stack = [source_slot0, 0xfeed_u64, 0xaaaa_u64, 0xbbbb_u64, 0xcccc_u64];

        let result = exec_iface_assert(
            stack.as_mut_ptr(),
            0,
            &inst,
            1,
            1,
            &[
                vo_runtime::SlotType::Interface0,
                vo_runtime::SlotType::Interface1,
            ],
            &mut itab_cache,
            &module,
        );

        assert!(matches!(result, ExecResult::JitError(_)));
        assert_eq!(stack[2..5], [0xaaaa, 0xbbbb, 0xcccc]);
    }

    #[test]
    fn exec_iface_assert_zero_sized_concrete_uses_logical_result_width() {
        for value_kind in [ValueKind::Array, ValueKind::Struct] {
            let mut module = Module::new("iface-assert-zero-sized".to_string());
            module.runtime_types.push(match value_kind {
                ValueKind::Array => RuntimeType::Array {
                    len: 0,
                    elem: vo_runtime::ValueRttid::new(0, ValueKind::Int64),
                },
                ValueKind::Struct => RuntimeType::Struct {
                    fields: Vec::new(),
                    meta_id: 0,
                },
                _ => unreachable!(),
            });
            let mut itab_cache = ItabCache::new();
            let source_slot0 = interface::pack_slot0(0, 0, value_kind);
            let backing = [0_u64; 2];
            let source_slot1 = backing.as_ptr() as u64;

            let single_flags =
                pack_iface_assert_flags(0, false, 0).expect("valid zero-sized assertion flags");
            let single = Instruction::with_flags(Opcode::IfaceAssert, single_flags, 2, 0, 0);
            let mut single_stack = [source_slot0, source_slot1, 0xfeed_u64];
            assert!(matches!(
                exec_iface_assert(
                    single_stack.as_mut_ptr(),
                    0,
                    &single,
                    0,
                    0,
                    &[],
                    &mut itab_cache,
                    &module
                ),
                ExecResult::FrameChanged
            ));
            assert_eq!(single_stack[2], 0xfeed, "{value_kind:?}");

            let comma_ok_flags =
                pack_iface_assert_flags(0, true, 0).expect("valid zero-sized comma-ok flags");
            let comma_ok = Instruction::with_flags(Opcode::IfaceAssert, comma_ok_flags, 2, 0, 0);
            let mut comma_ok_stack = [source_slot0, source_slot1, 0xfeed_u64, 0xbeef_u64];
            assert!(matches!(
                exec_iface_assert(
                    comma_ok_stack.as_mut_ptr(),
                    0,
                    &comma_ok,
                    0,
                    0,
                    &[],
                    &mut itab_cache,
                    &module
                ),
                ExecResult::FrameChanged
            ));
            assert_eq!(comma_ok_stack[2..], [1, 0xbeef], "{value_kind:?}");
        }
    }

    #[test]
    fn exec_iface_assert_uses_metadata_to_disambiguate_u16_target_mirror_collision() {
        let module = Module::new("iface-assert-target-boundary".to_string());
        let flags = pack_iface_assert_flags(0, true, 1).expect("comma-ok scalar assertion flags");
        let inst = Instruction::with_flags(Opcode::IfaceAssert, flags, 2, 0, u16::MAX);

        for target_id in [u32::from(u16::MAX), u32::from(u16::MAX) + 1] {
            let source_slot0 = interface::pack_slot0(0, target_id, ValueKind::Int64);
            let mut stack = [source_slot0, 0xfeed_u64, 0xaaaa_u64, 0xbbbb_u64];
            let mut itab_cache = ItabCache::new();
            assert!(matches!(
                exec_iface_assert(
                    stack.as_mut_ptr(),
                    0,
                    &inst,
                    0,
                    target_id,
                    &[vo_runtime::SlotType::Value],
                    &mut itab_cache,
                    &module,
                ),
                ExecResult::FrameChanged
            ));
            assert_eq!(stack[2..], [0xfeed, 1]);

            let colliding_target = if target_id == u32::from(u16::MAX) {
                target_id + 1
            } else {
                target_id - 1
            };
            let mut stack = [source_slot0, 0xfeed_u64, 0xaaaa_u64, 0xbbbb_u64];
            assert!(matches!(
                exec_iface_assert(
                    stack.as_mut_ptr(),
                    0,
                    &inst,
                    0,
                    colliding_target,
                    &[vo_runtime::SlotType::Value],
                    &mut itab_cache,
                    &module,
                ),
                ExecResult::FrameChanged
            ));
            assert_eq!(stack[2..], [0, 0]);
        }
    }
}
