//! Shared post-lowering contracts for JIT, OSR and Native AOT artifacts.

use crate::{JitArtifactMetadata, JitError, MAX_JIT_NATIVE_FRAME_BYTES};

fn verify_frame_bytes(requested_bytes: usize) -> Result<(), JitError> {
    if requested_bytes > MAX_JIT_NATIVE_FRAME_BYTES {
        return Err(JitError::NativeFrameLimitExceeded {
            limit_bytes: MAX_JIT_NATIVE_FRAME_BYTES,
            requested_bytes,
        });
    }
    Ok(())
}

/// Reject obviously oversized input before expensive machine lowering. The
/// final check below also includes alignment, spills and outgoing arguments.
pub(crate) fn verify_lowered_frame(context: &cranelift_codegen::Context) -> Result<(), JitError> {
    verify_frame_bytes(
        context
            .func
            .sized_stack_slots
            .values()
            .fold(0usize, |total, slot| {
                total.saturating_add(slot.size as usize)
            }),
    )
}

pub(crate) fn compiled_metadata(
    context: &cranelift_codegen::Context,
    name: &str,
) -> Result<JitArtifactMetadata, JitError> {
    let compiled = context
        .compiled_code()
        .ok_or_else(|| JitError::Internal(format!("missing compiled code for {name}")))?;
    let layout = compiled
        .buffer
        .frame_layout()
        .ok_or_else(|| JitError::Internal(format!("missing native frame layout for {name}")))?;
    // SP-to-FP includes all spills, explicit slots, clobbers and outgoing
    // arguments. Reserve the two-word frame setup area on supported 64-bit
    // native targets, including artifacts without GC stack maps.
    verify_frame_bytes((layout.frame_to_fp_offset as usize).saturating_add(16))?;

    let source_locs = compiled.buffer.get_srclocs_sorted();
    let mut source_index = 0;
    let stack_maps = compiled.buffer.user_stack_maps();
    let mut entries = Vec::with_capacity(stack_maps.len());
    for (return_address, frame_size, map) in stack_maps {
        while source_locs
            .get(source_index)
            .is_some_and(|source| source.end < *return_address)
        {
            source_index += 1;
        }
        let source = source_locs
            .get(source_index)
            .filter(|source| source.start < *return_address && *return_address <= source.end)
            .ok_or_else(|| {
                JitError::Internal(format!(
                    "native stack map for {name} has no safepoint source location"
                ))
            })?;
        let id = source.loc.bits().checked_sub(1).ok_or_else(|| {
            JitError::Internal(format!(
                "native stack map for {name} has an invalid source location"
            ))
        })?;
        entries.push((id, *return_address, *frame_size, map.entries().collect()));
    }
    // Current exits emit their recovery stores directly. There are no emitted
    // JitResult::Deopt sites, so copying every compile-time frame state into
    // each published tier would retain unused, potentially quadratic metadata.
    JitArtifactMetadata::from_entries(compiled.code_info().total_size as usize, entries, name)
}
