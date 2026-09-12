//! Compact execution layouts derived from verified instruction metadata.
//!
//! Serialized instruction metadata remains the semantic authority. A loaded
//! module derives this fixed-width view once, allowing the interpreter to
//! consume already-validated scalar layout facts without repeating enum,
//! vector-shape, and width validation in every container operation.

#[cfg(not(feature = "std"))]
use alloc::{collections::BTreeMap, sync::Arc, vec::Vec};
#[cfg(feature = "std")]
use std::{collections::BTreeMap, sync::Arc, vec::Vec};

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

/// The interpreter's immutable layout facts for one verified function.
/// A frame transition selects one record; pointer and container instructions
/// retain direct access without repeated function-table lookups.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionExecutionLayouts {
    pointers: FunctionPointerLayouts,
    elements: FunctionElementLayouts,
}

impl FunctionExecutionLayouts {
    #[inline(always)]
    pub fn pointers(&self) -> &FunctionPointerLayouts {
        &self.pointers
    }

    #[inline(always)]
    pub fn elements(&self) -> &FunctionElementLayouts {
        &self.elements
    }
}

/// Function-indexed facts derived together from one immutable module image.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecutionLayoutMaps {
    functions: Arc<[FunctionExecutionLayouts]>,
}

impl ExecutionLayoutMaps {
    pub(crate) fn build(module: &Module, exact_bases: &ExactBaseMaps) -> Self {
        let functions = module
            .functions
            .iter()
            .enumerate()
            .map(|(func_id, function)| {
                let bases = exact_bases
                    .function(func_id as u32)
                    .expect("exact-base facts cover every module function");
                let mut pointers = Vec::with_capacity(function.instruction_metadata.len());
                let mut elements = Vec::with_capacity(function.instruction_metadata.len());
                for (pc, metadata) in function.instruction_metadata.iter().enumerate() {
                    pointers.push(
                        metadata
                            .ptr_value_layout()
                            .map(|layout| {
                                CompactPointerLayout::from_layout(layout, bases.write_barrier(pc))
                            })
                            .unwrap_or_default(),
                    );
                    elements.push(
                        metadata
                            .elem_layout()
                            .map(CompactElementLayout::from_layout)
                            .unwrap_or_default(),
                    );
                }
                FunctionExecutionLayouts {
                    pointers: FunctionPointerLayouts { entries: pointers },
                    elements: FunctionElementLayouts { entries: elements },
                }
            })
            .collect();
        Self { functions }
    }

    #[inline]
    pub fn function(&self, func_id: u32) -> Option<&FunctionExecutionLayouts> {
        self.functions.get(func_id as usize)
    }

    pub(crate) fn pointer_maps(&self) -> PointerLayoutMaps {
        PointerLayoutMaps {
            layouts: self.clone(),
        }
    }

    pub(crate) fn element_maps(&self) -> ElementLayoutMaps {
        ElementLayoutMaps {
            layouts: self.clone(),
        }
    }
}

/// Compatible pointer-only view of the shared immutable function records.
#[derive(Debug, Clone)]
pub struct PointerLayoutMaps {
    layouts: ExecutionLayoutMaps,
}

impl PointerLayoutMaps {
    #[inline]
    pub fn function(&self, func_id: u32) -> Option<&FunctionPointerLayouts> {
        self.layouts
            .function(func_id)
            .map(FunctionExecutionLayouts::pointers)
    }
}

impl PartialEq for PointerLayoutMaps {
    fn eq(&self, other: &Self) -> bool {
        self.layouts.functions.iter().map(|f| &f.pointers).eq(other
            .layouts
            .functions
            .iter()
            .map(|f| &f.pointers))
    }
}
impl Eq for PointerLayoutMaps {}

/// Compatible element-only view of the same records; cloning a view does not
/// copy its facts or create a second source of layout authority.
#[derive(Debug, Clone)]
pub struct ElementLayoutMaps {
    layouts: ExecutionLayoutMaps,
}

impl ElementLayoutMaps {
    #[inline]
    pub fn function(&self, func_id: u32) -> Option<&FunctionElementLayouts> {
        self.layouts
            .function(func_id)
            .map(FunctionExecutionLayouts::elements)
    }
}

impl PartialEq for ElementLayoutMaps {
    fn eq(&self, other: &Self) -> bool {
        self.layouts.functions.iter().map(|f| &f.elements).eq(other
            .layouts
            .functions
            .iter()
            .map(|f| &f.elements))
    }
}
impl Eq for ElementLayoutMaps {}

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

        let exact_bases = ExactBaseMaps::conservative(&module);
        let maps = ExecutionLayoutMaps::build(&module, &exact_bases);
        let layouts = maps.function(0).expect("function layouts").elements();
        assert!(maps.function(1).is_none());
        assert_eq!(layouts.get(4), None);
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

        let pointer_layouts = maps.function(0).expect("function layouts").pointers();
        assert_eq!(pointer_layouts.get(0), None);
        assert_eq!(pointer_layouts.get(4), None);
        let pointer_view = maps.pointer_maps();
        let element_view = maps.element_maps();
        assert!(core::ptr::eq(
            pointer_view.function(0).unwrap(),
            pointer_layouts
        ));
        assert!(core::ptr::eq(element_view.function(0).unwrap(), layouts));
        assert!(pointer_view.function(1).is_none());
        assert!(element_view.function(1).is_none());
        assert_eq!(
            pointer_layouts.get(3),
            Some(PointerExecutionLayout {
                value_slots: 1,
                needs_write_barrier: true,
                supports_exact_barrier: true,
                base_provenance: WriteBarrierBaseProvenance::UNKNOWN,
            })
        );
        module.functions[0].instruction_metadata[0] = InstructionMetadata::None;
        let other = ExecutionLayoutMaps::build(&module, &exact_bases);
        assert_eq!(pointer_view, other.pointer_maps());
        assert_ne!(element_view, other.element_maps());
        drop(maps);
        assert_eq!(
            pointer_view
                .function(0)
                .unwrap()
                .get(3)
                .unwrap()
                .value_slots,
            1
        );
        assert_eq!(element_view.function(0).unwrap().get(0).unwrap().bytes, 8);
    }
}

/// Immutable, interned select payload layouts. Waiting snapshots can clone an
/// Arc without copying or charging module metadata to every Fiber execution.
#[derive(Debug)]
pub struct SelectLayoutMaps {
    functions: Vec<Vec<(u32, Arc<Vec<SlotType>>)>>,
}

impl SelectLayoutMaps {
    pub(crate) fn build(module: &Module) -> Self {
        let mut interned = BTreeMap::<Vec<u8>, Arc<Vec<SlotType>>>::new();
        let functions = module
            .functions
            .iter()
            .map(|function| {
                function
                    .code
                    .iter()
                    .enumerate()
                    .filter_map(|(pc, instruction)| {
                        if !matches!(
                            instruction.opcode(),
                            crate::instruction::Opcode::SelectSend
                                | crate::instruction::Opcode::SelectRecv
                        ) {
                            return None;
                        }
                        let layout = function.instruction_metadata.get(pc)?.queue_elem_layout()?;
                        let key = layout.iter().map(|ty| *ty as u8).collect::<Vec<_>>();
                        let shared = interned
                            .entry(key)
                            .or_insert_with(|| Arc::new(layout.to_vec()))
                            .clone();
                        Some((pc as u32, shared))
                    })
                    .collect()
            })
            .collect();
        Self { functions }
    }

    #[inline]
    pub fn get(&self, function: u32, pc: u32) -> Option<&Arc<Vec<SlotType>>> {
        let entries = self.functions.get(function as usize)?;
        let index = entries.binary_search_by_key(&pc, |entry| entry.0).ok()?;
        Some(&entries[index].1)
    }
}
