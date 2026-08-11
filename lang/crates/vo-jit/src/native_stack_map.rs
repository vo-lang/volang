//! Owned metadata for precise roots spilled by Cranelift at native safepoints.
//!
//! Cranelift owns stack maps only while a compilation context is alive.  JIT
//! artifacts outlive that context, so the runtime-facing form deliberately
//! contains no Cranelift references and is retained beside the code pointer.

use cranelift_codegen::ir::{types, Type};

use crate::JitError;

/// Root representation understood by the Volang collector.
///
/// Interface pairs are added through a separate, conditional root-area map in
/// the native-frame phase.  Cranelift's scalar stack maps currently carry the
/// direct `GcRef` roots described here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeRootKind {
    GcRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NativeStackRoot {
    /// Byte offset from the machine stack pointer at the safepoint.
    pub sp_offset: u32,
    pub kind: NativeRootKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeStackMap {
    /// Dense identifier stored in `JitNativeFrame` while the call is active.
    pub safepoint_id: u32,
    /// Offset of the return address immediately after the safepoint call.
    pub return_address_offset: u32,
    /// Active native frame size reported by Cranelift for this safepoint.
    pub frame_size: u32,
    /// SP-relative location of the active `JitNativeFrame` record.
    pub anchor_sp_offset: u32,
    /// Conditional roots such as interface payloads require typed VM-frame
    /// materialization before collection.
    pub requires_frame_materialization: bool,
    pub roots: Box<[NativeStackRoot]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JitArtifactMetadata {
    pub code_size: u32,
    pub stack_maps: Box<[NativeStackMap]>,
    safepoint_index: Box<[u32]>,
}

impl JitArtifactMetadata {
    pub(crate) fn from_entries(
        code_size: usize,
        entries: impl IntoIterator<Item = (u32, u32, u32, Vec<(Type, u32)>)>,
        name: &str,
    ) -> Result<Self, JitError> {
        let code_size = u32::try_from(code_size).map_err(|_| {
            JitError::Internal(format!(
                "native code for {name} exceeds the u32 metadata range"
            ))
        })?;
        let mut maps = Vec::new();
        let mut previous_return_address = None;

        for (safepoint_id, return_address_offset, frame_size, entries) in entries {
            if return_address_offset > code_size {
                return Err(JitError::Internal(format!(
                    "native stack map for {name} points outside its code: {} > {code_size}",
                    return_address_offset
                )));
            }
            if previous_return_address.is_some_and(|previous| previous >= return_address_offset) {
                return Err(JitError::Internal(format!(
                    "native stack maps for {name} are not strictly ordered"
                )));
            }
            previous_return_address = Some(return_address_offset);

            let mut roots = Vec::new();
            let mut anchor_sp_offset = None;
            let mut requires_frame_materialization = false;
            for (ty, sp_offset) in entries {
                if ty == types::I32 {
                    continue;
                }
                if ty == types::I8 {
                    if anchor_sp_offset.replace(sp_offset).is_some() {
                        return Err(JitError::Internal(format!(
                            "native stack map for {name} contains multiple frame anchors"
                        )));
                    }
                    continue;
                }
                if ty == types::I16 {
                    if requires_frame_materialization {
                        return Err(JitError::Internal(format!(
                            "native stack map for {name} contains multiple materialization markers"
                        )));
                    }
                    requires_frame_materialization = true;
                    continue;
                }
                let kind = root_kind_for_type(ty).ok_or_else(|| {
                    JitError::Internal(format!(
                        "native stack map for {name} contains unsupported root type {ty}"
                    ))
                })?;
                let width = ty.bytes();
                if sp_offset
                    .checked_add(width)
                    .is_none_or(|end| end > frame_size)
                {
                    return Err(JitError::Internal(format!(
                        "native stack map for {name} has root [{sp_offset}, {}) outside frame size {frame_size}",
                        sp_offset.saturating_add(width)
                    )));
                }
                roots.push(NativeStackRoot { sp_offset, kind });
            }
            roots.sort_unstable_by_key(|root| root.sp_offset);
            if roots
                .windows(2)
                .any(|pair| pair[0].sp_offset == pair[1].sp_offset)
            {
                return Err(JitError::Internal(format!(
                    "native stack map for {name} contains duplicate root offsets"
                )));
            }
            let anchor_sp_offset = anchor_sp_offset.ok_or_else(|| {
                JitError::Internal(format!("native stack map for {name} has no frame anchor"))
            })?;
            if anchor_sp_offset >= frame_size {
                return Err(JitError::Internal(format!(
                    "native stack map for {name} has anchor {anchor_sp_offset} outside frame size {frame_size}"
                )));
            }

            maps.push(NativeStackMap {
                safepoint_id,
                return_address_offset,
                frame_size,
                anchor_sp_offset,
                requires_frame_materialization,
                roots: roots.into_boxed_slice(),
            });
        }

        let index_len = maps
            .iter()
            .map(|map| map.safepoint_id)
            .max()
            .map_or(0usize, |max| max as usize + 1);
        let mut safepoint_index = vec![u32::MAX; index_len];
        for (map_index, map) in maps.iter().enumerate() {
            let entry = &mut safepoint_index[map.safepoint_id as usize];
            if *entry != u32::MAX {
                return Err(JitError::Internal(format!(
                    "native stack maps for {name} contain duplicate safepoint id {}",
                    map.safepoint_id
                )));
            }
            *entry = u32::try_from(map_index)
                .map_err(|_| JitError::Internal("native stack map index overflow".into()))?;
        }

        Ok(Self {
            code_size,
            stack_maps: maps.into_boxed_slice(),
            safepoint_index: safepoint_index.into_boxed_slice(),
        })
    }

    pub fn retained_bytes(&self) -> usize {
        core::mem::size_of::<Self>()
            .saturating_add(
                self.stack_maps
                    .len()
                    .saturating_mul(core::mem::size_of::<NativeStackMap>()),
            )
            .saturating_add(
                self.stack_maps
                    .iter()
                    .map(|map| {
                        map.roots
                            .len()
                            .saturating_mul(core::mem::size_of::<NativeStackRoot>())
                    })
                    .sum::<usize>(),
            )
            .saturating_add(
                self.safepoint_index
                    .len()
                    .saturating_mul(core::mem::size_of::<u32>()),
            )
    }

    pub fn map_for_return_address_offset(&self, offset: u32) -> Option<&NativeStackMap> {
        self.stack_maps
            .binary_search_by_key(&offset, |map| map.return_address_offset)
            .ok()
            .map(|index| &self.stack_maps[index])
    }

    pub fn map_for_safepoint_id(&self, safepoint_id: u32) -> Option<&NativeStackMap> {
        let index = *self.safepoint_index.get(safepoint_id as usize)?;
        (index != u32::MAX).then(|| &self.stack_maps[index as usize])
    }
}

fn root_kind_for_type(ty: Type) -> Option<NativeRootKind> {
    (ty == types::I64).then_some(NativeRootKind::GcRef)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn artifact_metadata_lookup_is_exact_and_ordered() {
        let metadata = JitArtifactMetadata {
            code_size: 64,
            stack_maps: vec![
                NativeStackMap {
                    safepoint_id: 0,
                    return_address_offset: 12,
                    frame_size: 32,
                    anchor_sp_offset: 8,
                    requires_frame_materialization: false,
                    roots: Box::new([]),
                },
                NativeStackMap {
                    safepoint_id: 1,
                    return_address_offset: 40,
                    frame_size: 48,
                    anchor_sp_offset: 16,
                    requires_frame_materialization: false,
                    roots: Box::new([]),
                },
            ]
            .into_boxed_slice(),
            safepoint_index: vec![0, 1].into_boxed_slice(),
        };

        assert_eq!(
            metadata
                .map_for_return_address_offset(40)
                .map(|map| map.frame_size),
            Some(48)
        );
        assert!(metadata.map_for_return_address_offset(39).is_none());
        assert_eq!(
            metadata
                .map_for_safepoint_id(1)
                .map(|map| map.return_address_offset),
            Some(40)
        );
    }

    #[test]
    fn retained_bytes_includes_maps_and_roots() {
        let metadata = JitArtifactMetadata {
            code_size: 1,
            stack_maps: vec![NativeStackMap {
                safepoint_id: 0,
                return_address_offset: 1,
                frame_size: 8,
                anchor_sp_offset: 0,
                requires_frame_materialization: false,
                roots: vec![NativeStackRoot {
                    sp_offset: 0,
                    kind: NativeRootKind::GcRef,
                }]
                .into_boxed_slice(),
            }]
            .into_boxed_slice(),
            safepoint_index: vec![0].into_boxed_slice(),
        };

        assert!(metadata.retained_bytes() >= core::mem::size_of::<JitArtifactMetadata>());
        assert!(metadata.retained_bytes() > core::mem::size_of::<NativeStackMap>());
    }

    #[test]
    fn conditional_root_marker_requires_vm_frame_materialization() {
        let metadata = JitArtifactMetadata::from_entries(
            16,
            [(0, 8, 32, vec![(types::I8, 4), (types::I16, 4)])],
            "conditional",
        )
        .expect("conditional stack map");

        let map = metadata.map_for_safepoint_id(0).expect("safepoint");
        assert!(map.requires_frame_materialization);
        assert!(map.roots.is_empty());
    }
}
