//! Compact execution layouts derived from verified instruction metadata.
//!
//! Serialized instruction metadata remains the semantic authority. A loaded
//! module derives this fixed-width view once, allowing the interpreter to
//! consume already-validated scalar layout facts without repeating enum,
//! vector-shape, and width validation in every container operation.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::vec::Vec;

use crate::bytecode::{ElemLayout, Module};
use crate::exact_bases::{ExactBaseMaps, WriteBarrierBaseProvenance};
use crate::SlotType;

const PRESENT: u8 = 1;

#[repr(C)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct CompactElementLayout {
    bytes: u32,
    slots: u16,
    needs_sign_extend: u8,
    present: u8,
}

impl CompactElementLayout {
    fn from_layout(layout: ElemLayout) -> Self {
        Self {
            bytes: u32::try_from(layout.bytes)
                .expect("instruction element byte width originates from u32 metadata"),
            slots: layout.slots,
            needs_sign_extend: u8::from(layout.needs_sign_extend),
            present: PRESENT,
        }
    }

    #[inline]
    fn get(self) -> Option<ElemLayout> {
        (self.present == PRESENT).then_some(ElemLayout {
            bytes: self.bytes as usize,
            slots: self.slots,
            needs_sign_extend: self.needs_sign_extend != 0,
        })
    }
}

/// Fixed-width per-PC element layout facts for one function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionElementLayouts {
    entries: Vec<CompactElementLayout>,
}

impl FunctionElementLayouts {
    #[inline]
    pub fn get(&self, pc: usize) -> Option<ElemLayout> {
        self.entries.get(pc).copied()?.get()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// Scalar pointer execution facts consumed by the verified interpreter.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct PointerExecutionLayout {
    pub value_slots: u16,
    pub needs_write_barrier: bool,
    pub supports_exact_barrier: bool,
    pub base_provenance: WriteBarrierBaseProvenance,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct CompactPointerLayout {
    value_slots: u16,
    needs_write_barrier: u8,
    supports_exact_barrier: u8,
    base_provenance: WriteBarrierBaseProvenance,
    present: u8,
}

impl CompactPointerLayout {
    fn from_layout(value_layout: &[SlotType], base_provenance: WriteBarrierBaseProvenance) -> Self {
        let first = value_layout.first();
        Self {
            value_slots: u16::try_from(value_layout.len())
                .expect("verified pointer layout width fits u16"),
            needs_write_barrier: u8::from(first.is_some_and(|slot| slot.needs_write_barrier())),
            supports_exact_barrier: u8::from(matches!(
                first,
                Some(SlotType::GcBase | SlotType::GcRef)
            )),
            base_provenance,
            present: PRESENT,
        }
    }

    #[inline]
    fn get(self) -> Option<PointerExecutionLayout> {
        (self.present == PRESENT).then_some(PointerExecutionLayout {
            value_slots: self.value_slots,
            needs_write_barrier: self.needs_write_barrier != 0,
            supports_exact_barrier: self.supports_exact_barrier != 0,
            base_provenance: self.base_provenance,
        })
    }
}

/// Fixed-width per-PC pointer facts for one function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionPointerLayouts {
    entries: Vec<CompactPointerLayout>,
}

impl FunctionPointerLayouts {
    #[inline]
    pub fn get(&self, pc: usize) -> Option<PointerExecutionLayout> {
        self.entries.get(pc).copied()?.get()
    }

    /// Consume pointer facts whose presence and PC range were established by
    /// module verification.
    ///
    /// # Safety
    /// `pc` must identify an instruction that requires `PtrLayout` metadata in
    /// the function from which this map was derived.
    #[inline(always)]
    pub unsafe fn get_verified(&self, pc: usize) -> PointerExecutionLayout {
        debug_assert!(pc < self.entries.len());
        let entry = unsafe { *self.entries.get_unchecked(pc) };
        debug_assert_eq!(entry.present, PRESENT);
        PointerExecutionLayout {
            value_slots: entry.value_slots,
            needs_write_barrier: entry.needs_write_barrier != 0,
            supports_exact_barrier: entry.supports_exact_barrier != 0,
            base_provenance: entry.base_provenance,
        }
    }
}

/// Pointer execution facts bound to the exact loaded module image.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerLayoutMaps {
    functions: Vec<FunctionPointerLayouts>,
}

impl PointerLayoutMaps {
    pub(crate) fn build(module: &Module, exact_bases: &ExactBaseMaps) -> Self {
        let functions = module
            .functions
            .iter()
            .enumerate()
            .map(|(func_id, function)| {
                let bases = exact_bases
                    .function(func_id as u32)
                    .expect("exact-base facts cover every module function");
                FunctionPointerLayouts {
                    entries: function
                        .instruction_metadata
                        .iter()
                        .enumerate()
                        .map(|(pc, metadata)| {
                            metadata
                                .ptr_value_layout()
                                .map(|layout| {
                                    CompactPointerLayout::from_layout(
                                        layout,
                                        bases.write_barrier(pc),
                                    )
                                })
                                .unwrap_or_default()
                        })
                        .collect(),
                }
            })
            .collect();
        Self { functions }
    }

    #[inline]
    pub fn function(&self, func_id: u32) -> Option<&FunctionPointerLayouts> {
        self.functions.get(func_id as usize)
    }
}

/// Element-layout execution facts bound to the exact loaded module image.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElementLayoutMaps {
    functions: Vec<FunctionElementLayouts>,
}

impl ElementLayoutMaps {
    pub(crate) fn build(module: &Module) -> Self {
        let functions = module
            .functions
            .iter()
            .map(|function| FunctionElementLayouts {
                entries: function
                    .instruction_metadata
                    .iter()
                    .map(|metadata| {
                        metadata
                            .elem_layout()
                            .map(CompactElementLayout::from_layout)
                            .unwrap_or_default()
                    })
                    .collect(),
            })
            .collect();
        Self { functions }
    }

    #[inline]
    pub fn function(&self, func_id: u32) -> Option<&FunctionElementLayouts> {
        self.functions.get(func_id as usize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::{FunctionDef, InstructionMetadata};
    use crate::SlotType;

    #[test]
    fn maps_valid_layouts_and_rejects_invalid_shapes() {
        let mut module = Module::new("execution-layouts".to_string());
        let function = FunctionDef {
            name: "f".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: 0,
            ret_slots: 0,
            ret_slot_types: Vec::new(),
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: Vec::new(),
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: Vec::new(),
            instruction_metadata: vec![
                InstructionMetadata::ElemLayout {
                    elem_bytes: 8,
                    needs_sign_extend: false,
                    slot_layout: vec![SlotType::Value],
                },
                InstructionMetadata::ElemLayout {
                    elem_bytes: 9,
                    needs_sign_extend: false,
                    slot_layout: vec![SlotType::Value],
                },
                InstructionMetadata::None,
                InstructionMetadata::PtrLayout {
                    value_layout: vec![SlotType::GcBase],
                },
            ],
            slot_types: Vec::new(),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        };
        module.functions.push(function);

        let maps = ElementLayoutMaps::build(&module);
        let layouts = maps.function(0).expect("function layouts");
        assert_eq!(
            layouts.get(0),
            Some(ElemLayout {
                bytes: 8,
                slots: 1,
                needs_sign_extend: false,
            })
        );
        assert_eq!(layouts.get(1), None);
        assert_eq!(layouts.get(2), None);

        let exact_bases = ExactBaseMaps::conservative(&module);
        let pointer_maps = PointerLayoutMaps::build(&module, &exact_bases);
        let pointer_layouts = pointer_maps.function(0).expect("pointer layouts");
        assert_eq!(
            pointer_layouts.get(3),
            Some(PointerExecutionLayout {
                value_slots: 1,
                needs_write_barrier: true,
                supports_exact_barrier: true,
                base_provenance: WriteBarrierBaseProvenance::UNKNOWN,
            })
        );
    }
}
