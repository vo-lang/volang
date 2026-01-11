//! Embedding path analysis for method resolution.
//!
//! This module provides a unified way to analyze embedding paths for:
//! - Direct method calls (compile_concrete_method)
//! - Itab building (build_itab)
//! - Wrapper generation
//!
//! The key insight is that all method calls through embedding share the same
//! path analysis logic, whether at compile time or for runtime dispatch.

use vo_analysis::objects::{TCObjects, TypeKey};
use vo_analysis::check::type_info as layout;
use vo_analysis::typ::{self, Type};

/// Result of analyzing an embedding path.
#[derive(Debug, Clone)]
pub struct EmbedPathInfo {
    /// Steps in the embedding path (empty = direct method, no embedding)
    pub steps: Vec<EmbedStep>,
    /// Total byte offset to reach the final embedded field
    pub total_offset: u16,
    /// The final type after traversing the path
    pub final_type: TypeKey,
    /// If the path ends at an interface field, this contains the interface info
    pub embedded_iface: Option<EmbeddedIfaceInfo>,
}

/// A single step in an embedding path.
#[derive(Debug, Clone, Copy)]
pub struct EmbedStep {
    /// True if this field is a pointer type (*T)
    pub is_pointer: bool,
}

/// Info about an embedded interface at the end of the path.
#[derive(Debug, Clone, Copy)]
pub struct EmbeddedIfaceInfo {
    /// Offset to the interface field
    pub offset: u16,
    /// The interface type
    pub iface_type: TypeKey,
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
    let mut total_offset = 0u16;
    let mut current_type = recv_type;
    let mut embedded_iface = None;
    
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
        
        steps.push(EmbedStep { is_pointer });
        
        total_offset += field_offset;
        
        if is_interface {
            embedded_iface = Some(EmbeddedIfaceInfo {
                offset: total_offset,
                iface_type: field_type,
            });
            break;
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
    
    EmbedPathInfo {
        steps,
        total_offset,
        final_type: current_type,
        embedded_iface,
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
    EmbeddedInterface {
        embed_offset: u16,
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
                total_offset: 0,
                final_type: recv_type,
                embedded_iface: None,
            },
            recv_is_pointer,
        });
    }
    
    // Get indices from selection
    let indices = selection.map(|s| s.indices()).unwrap_or(&[]);
    
    // Analyze embedding path
    let embed_path = analyze_embed_path(base_type, indices, tc_objs);
    
    // Case 2: Embedded interface - method comes from interface field
    if let Some(embed_iface) = embed_path.embedded_iface {
        let method_idx = ctx.get_interface_method_index(embed_iface.iface_type, method_name, tc_objs, interner);
        return Some(MethodCallInfo {
            dispatch: MethodDispatch::EmbeddedInterface {
                embed_offset: embed_iface.offset,
                iface_type: embed_iface.iface_type,
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

#[cfg(test)]
mod tests {
    // Unit tests would go here
}
