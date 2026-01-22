//! Embedding path analysis and traversal for method resolution.
//!
//! # Overview
//! 
//! When a struct embeds another type, methods are "promoted" and can be called
//! on the outer type. This module handles:
//! 
//! 1. **Path Analysis** - Analyze embedding indices to build `EmbedPathInfo`
//! 2. **Method Resolution** - Determine dispatch strategy (`MethodDispatch`)
//! 3. **Code Generation** - Emit instructions to traverse the path (`emit_embed_path_traversal`)
//!
//! # Key Types
//!
//! - `EmbedPathInfo` - Analyzed path with cached `total_offset` and `has_pointer_step`
//! - `EmbedStep` - Single step: `{is_pointer, offset}`
//! - `MethodCallInfo` - Complete info for compiling a method call
//! - `MethodDispatch` - How to dispatch: Static, Interface, or EmbeddedInterface
//!
//! # Path Traversal Logic
//!
//! For `Outer.Inner.Method()` where Inner is embedded:
//! - If no pointer steps: use `total_offset` directly (fast path)
//! - If has pointer steps: traverse step by step, dereference pointers

use vo_analysis::objects::{TCObjects, TypeKey};
use vo_analysis::check::type_info as layout;
use vo_analysis::typ::{self, Type};

/// Result of analyzing an embedding path.
#[derive(Debug, Clone)]
pub struct EmbedPathInfo {
    /// Steps in the embedding path (empty = direct method, no embedding)
    pub steps: Vec<EmbedStep>,
    /// The final type after traversing the path
    pub final_type: TypeKey,
    /// Sum of all step offsets (valid only when no pointer steps, otherwise use traversal)
    pub total_offset: u16,
    /// True if any step is a pointer type (requires traversal logic)
    pub has_pointer_step: bool,
    /// Number of slots for the final type (2 for interface, computed for structs)
    pub final_slots: u16,
    /// If the path ends at an interface field, this is the interface type
    pub embedded_iface_type: Option<TypeKey>,
}

/// A single step in an embedding path.
#[derive(Debug, Clone, Copy)]
pub struct EmbedStep {
    /// True if this field is a pointer type (*T)
    pub is_pointer: bool,
    /// Offset of this field within its containing struct
    pub offset: u16,
}

/// Analyze an embedding path from indices (as returned by Selection or lookup).
///
/// The indices array is [field_idx, ..., method_idx] where:
/// - All but the last element are field indices for embedded structs/interfaces
/// - The last element is the method index (ignored in path analysis)
///
/// This function is the single source of truth for embedding path analysis.
pub fn analyze_embed_path(
    recv_type: TypeKey,
    indices: &[usize],
    tc_objs: &TCObjects,
) -> EmbedPathInfo {
    // Walk through all indices except the last one (which is method index)
    let path_indices = if indices.len() > 1 {
        &indices[..indices.len() - 1]
    } else {
        &[]
    };
    analyze_embed_path_impl(recv_type, path_indices, tc_objs)
}

/// Analyze embedding path using ALL indices (no method index skipping).
/// Used by wrapper generation where indices are already stripped.
pub fn analyze_embed_path_raw(
    recv_type: TypeKey,
    indices: &[usize],
    tc_objs: &TCObjects,
) -> EmbedPathInfo {
    analyze_embed_path_impl(recv_type, indices, tc_objs)
}

/// Core implementation for embedding path analysis.
fn analyze_embed_path_impl(
    recv_type: TypeKey,
    indices: &[usize],
    tc_objs: &TCObjects,
) -> EmbedPathInfo {
    let mut steps = Vec::new();
    let mut current_type = recv_type;
    
    // Strip pointer from receiver if needed
    if layout::is_pointer(current_type, tc_objs) {
        let underlying = typ::underlying_type(current_type, tc_objs);
        if let Type::Pointer(p) = &tc_objs.types[underlying] {
            current_type = p.base();
        }
    }
    
    for &idx in indices {
        let (field_offset, _slots) = layout::struct_field_offset_by_index(current_type, idx, tc_objs);
        let field_type = layout::struct_field_type_by_index(current_type, idx, tc_objs);
        
        let is_pointer = layout::is_pointer(field_type, tc_objs);
        let is_interface = layout::is_interface(field_type, tc_objs);
        
        steps.push(EmbedStep { is_pointer, offset: field_offset });
        
        if is_interface {
            // For embedded interface, calculate total offset from steps
            let total_offset: u16 = steps.iter().map(|s| s.offset).sum();
            let has_pointer_step = steps.iter().any(|s| s.is_pointer);
            return EmbedPathInfo {
                steps,
                final_type: field_type,
                total_offset,
                has_pointer_step,
                final_slots: 2,  // interface is always 2 slots
                embedded_iface_type: Some(field_type),
            };
        }
        
        if is_pointer {
            let underlying = typ::underlying_type(field_type, tc_objs);
            if let Type::Pointer(p) = &tc_objs.types[underlying] {
                current_type = p.base();
            }
        } else {
            current_type = field_type;
        }
    }
    
    let total_offset = steps.iter().map(|s| s.offset).sum();
    let has_pointer_step = steps.iter().any(|s| s.is_pointer);
    let final_slots = layout::type_slot_count(current_type, tc_objs);
    
    EmbedPathInfo {
        steps,
        final_type: current_type,
        total_offset,
        has_pointer_step,
        final_slots,
        embedded_iface_type: None,
    }
}

// =============================================================================
// Unified Method Call Resolution
// =============================================================================

/// How to dispatch a method call.
#[derive(Debug, Clone)]
pub enum MethodDispatch {
    /// Static call - func_id known at compile time
    Static {
        func_id: u32,
        expects_ptr_recv: bool,
    },
    /// Interface dispatch - lookup method at runtime via itab
    Interface {
        method_idx: u32,
    },
    /// Embedded interface - load interface from struct field, then dispatch
    /// Note: offset info is in EmbedPathInfo, not here
    EmbeddedInterface {
        iface_type: TypeKey,
        method_idx: u32,
    },
}

/// Complete information for compiling a method call.
#[derive(Debug, Clone)]
pub struct MethodCallInfo {
    /// How to dispatch
    pub dispatch: MethodDispatch,
    /// Embedding path to receiver (empty = direct)
    pub embed_path: EmbedPathInfo,
    /// Whether the original receiver expression is a pointer type
    pub recv_is_pointer: bool,
}

impl MethodCallInfo {
    /// Whether method expects pointer receiver
    pub fn expects_ptr_recv(&self) -> bool {
        match &self.dispatch {
            MethodDispatch::Static { expects_ptr_recv, .. } => *expects_ptr_recv,
            _ => false,
        }
    }
    
    /// Get actual receiver type (after following embedding path)
    pub fn actual_recv_type(&self, base_type: TypeKey) -> TypeKey {
        if self.embed_path.steps.is_empty() {
            base_type
        } else {
            self.embed_path.final_type
        }
    }
    
    /// Emit code to extract the target (receiver or interface) for this method call.
    /// 
    /// This is the unified entry point for target extraction, handling all dispatch types:
    /// - Static: extract receiver (value or pointer based on method signature)
    /// - Interface: copy interface directly (no embedding)
    /// - EmbeddedInterface: traverse path and extract interface
    pub fn emit_target(
        &self,
        builder: &mut FuncBuilder,
        start: TraverseStart,
        dst: u16,
    ) {
        let (expects_ptr, slots) = match &self.dispatch {
            MethodDispatch::Static { expects_ptr_recv, .. } => {
                (*expects_ptr_recv, self.embed_path.final_slots)
            }
            MethodDispatch::EmbeddedInterface { .. } | MethodDispatch::Interface { .. } => {
                (false, 2)  // interface is always value type, 2 slots
            }
        };
        emit_embed_path_traversal(builder, start, &self.embed_path.steps, expects_ptr, slots, dst);
    }
}

/// Resolve a method call from Selection info.
/// 
/// This is the unified entry point for method call resolution.
/// It handles all cases:
/// - Direct method on type
/// - Promoted method through struct embedding
/// - Method from embedded interface
/// - Interface method dispatch
pub fn resolve_method_call(
    recv_type: TypeKey,
    method_name: &str,
    method_sym: vo_common::Symbol,
    selection: Option<&vo_analysis::selection::Selection>,
    is_interface_recv: bool,
    ctx: &mut crate::context::CodegenContext,
    tc_objs: &TCObjects,
    interner: &vo_common::SymbolInterner,
) -> Option<MethodCallInfo> {
    let recv_is_pointer = layout::is_pointer(recv_type, tc_objs);
    let base_type = if recv_is_pointer {
        let underlying = typ::underlying_type(recv_type, tc_objs);
        if let Type::Pointer(p) = &tc_objs.types[underlying] {
            p.base()
        } else {
            recv_type
        }
    } else {
        recv_type
    };
    
    // Case 1: Interface receiver - use interface dispatch
    if is_interface_recv {
        let method_idx = ctx.get_interface_method_index(recv_type, method_name, tc_objs, interner);
        return Some(MethodCallInfo {
            dispatch: MethodDispatch::Interface { method_idx },
            embed_path: EmbedPathInfo {
                steps: Vec::new(),
                final_type: recv_type,
                total_offset: 0,
                has_pointer_step: false,
                final_slots: 2,
                embedded_iface_type: None,
            },
            recv_is_pointer,
        });
    }
    
    // Get indices from selection
    let indices = selection.map(|s| s.indices()).unwrap_or(&[]);
    
    // Analyze embedding path
    let embed_path = analyze_embed_path(base_type, indices, tc_objs);
    
    // Case 2: Embedded interface - method comes from interface field
    if let Some(iface_type) = embed_path.embedded_iface_type {
        let method_idx = ctx.get_interface_method_index(iface_type, method_name, tc_objs, interner);
        return Some(MethodCallInfo {
            dispatch: MethodDispatch::EmbeddedInterface {
                iface_type,
                method_idx,
            },
            embed_path,
            recv_is_pointer,
        });
    }
    
    // Case 3: Concrete method (direct or promoted)
    // Try to find method on final_type (after following embedding path)
    let search_type = if embed_path.steps.is_empty() {
        base_type
    } else {
        embed_path.final_type
    };
    
    // Try value receiver
    if let Some(func_id) = ctx.get_func_index(Some(search_type), false, method_sym) {
        return Some(MethodCallInfo {
            dispatch: MethodDispatch::Static { func_id, expects_ptr_recv: false },
            embed_path,
            recv_is_pointer,
        });
    }
    
    // Try pointer receiver
    if let Some(func_id) = ctx.get_func_index(Some(search_type), true, method_sym) {
        return Some(MethodCallInfo {
            dispatch: MethodDispatch::Static { func_id, expects_ptr_recv: true },
            embed_path,
            recv_is_pointer,
        });
    }
    
    None
}

// =============================================================================
// Unified Embed Path Traversal for Code Generation
// =============================================================================

use crate::func::FuncBuilder;
use vo_runtime::SlotType;

/// Initial state for embed path traversal.
#[derive(Debug, Clone, Copy)]
pub struct TraverseStart {
    /// The register containing the initial value
    pub reg: u16,
    /// True if reg contains a GcRef, false if it's a stack value
    pub is_pointer: bool,
}

/// Emit instructions to traverse an embed path and extract the receiver.
///
/// # Logic
///
/// 1. Empty path → direct extraction via `emit_final_receiver`
/// 2. No pointer steps → use total_offset directly (fast path)
/// 3. Has pointer steps → iterate, dereference at each pointer step
///
/// # Final Receiver Extraction (`emit_final_receiver`)
///
/// | expects_ptr | is_pointer | offset | Action |
/// |-------------|------------|--------|--------|
/// | true        | true       | 0      | Copy pointer |
/// | true        | true       | >0     | PtrAdd(ptr, offset) |
/// | true        | false      | any    | Copy stack slot (pointer field) |
/// | false       | true       | any    | PtrGet(ptr, offset, slots) |
/// | false       | false      | any    | Copy stack slots |
pub fn emit_embed_path_traversal(
    builder: &mut FuncBuilder,
    start: TraverseStart,
    steps: &[EmbedStep],
    expects_ptr_recv: bool,
    recv_slots: u16,
    dst: u16,
) {
    // Empty path - no embedding
    if steps.is_empty() {
        emit_final_receiver(builder, start.reg, start.is_pointer, 0, expects_ptr_recv, recv_slots, dst);
        return;
    }
    
    let has_pointer = steps.iter().any(|s| s.is_pointer);
    let total_offset: u16 = steps.iter().map(|s| s.offset).sum();
    
    // Fast path: no pointer steps
    if !has_pointer {
        emit_final_receiver(builder, start.reg, start.is_pointer, total_offset, expects_ptr_recv, recv_slots, dst);
        return;
    }
    
    // General case: traverse pointer chain
    let mut current_is_ptr = start.is_pointer;
    let mut current_reg = start.reg;
    let mut accumulated_offset: u16 = 0;
    
    for (i, step) in steps.iter().enumerate() {
        accumulated_offset += step.offset;
        
        if step.is_pointer {
            // Read the pointer field
            let temp_ptr = builder.alloc_temp_typed(&[SlotType::GcRef]);
            if current_is_ptr {
                builder.emit_ptr_get(temp_ptr, current_reg, accumulated_offset, 1);
            } else {
                builder.emit_copy(temp_ptr, current_reg + accumulated_offset, 1);
            }
            current_reg = temp_ptr;
            current_is_ptr = true;
            accumulated_offset = 0;
        }
        
        // Emit final receiver at the last step
        if i == steps.len() - 1 {
            emit_final_receiver(builder, current_reg, current_is_ptr, accumulated_offset, expects_ptr_recv, recv_slots, dst);
        }
    }
}

/// Extract the final receiver based on pointer state and method expectation.
/// See `emit_embed_path_traversal` doc for the decision matrix.
fn emit_final_receiver(
    builder: &mut FuncBuilder,
    reg: u16,
    is_pointer: bool,
    offset: u16,
    expects_ptr_recv: bool,
    recv_slots: u16,
    dst: u16,
) {
    if expects_ptr_recv {
        // Method expects *T - need to produce a pointer to the embedded field
        if is_pointer {
            if offset == 0 {
                // No offset, just copy the pointer
                builder.emit_copy(dst, reg, 1);
            } else {
                // Need to compute ptr + offset to get pointer to embedded field
                let offset_reg = builder.alloc_temp_typed(&[SlotType::Value]);
                builder.emit_op(vo_vm::instruction::Opcode::LoadInt, offset_reg, offset, 0);
                builder.emit_ptr_add(dst, reg, offset_reg);
            }
        } else {
            // Stack value - copy the pointer field at offset
            builder.emit_copy(dst, reg + offset, 1);
        }
    } else {
        // Method expects T - dereference to get value
        if is_pointer {
            builder.emit_ptr_get(dst, reg, offset, recv_slots);
        } else {
            builder.emit_copy(dst, reg + offset, recv_slots);
        }
    }
}

#[cfg(test)]
mod tests {
    // Unit tests would go here
}
