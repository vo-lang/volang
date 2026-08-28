//! Owned metadata for precise roots spilled by Cranelift at native safepoints.
//!
//! Cranelift owns stack maps only while a compilation context is alive.  JIT
//! artifacts outlive that context, so the runtime-facing form deliberately
//! contains no Cranelift references and is retained beside the code pointer.

use cranelift_codegen::ir::{types, Type};

use crate::JitError;

/// Root representation understood by the Volang collector.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeRootKind {
    GcRef,
    /// Two adjacent words: a runtime type header followed by a payload. The
    /// payload is a root exactly when the header describes a managed value.
    InterfacePair,
}

impl NativeRootKind {
    pub const fn width(self) -> u32 {
        match self {
            Self::GcRef => 8,
            Self::InterfacePair => 16,
        }
    }
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
    pub roots: Box<[NativeStackRoot]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum DeoptValueKind {
    Word = 0,
    Float64 = 1,
    GcRef = 2,
    InterfaceHeader = 3,
    InterfaceData = 4,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeoptValueLocation {
    /// The value has been materialized in the canonical fiber frame.
    FiberSlot(u16),
    /// Reserved for constant folding and virtual-object materialization.
    Constant(u64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DeoptValue {
    pub slot: u16,
    pub kind: DeoptValueKind,
    pub location: DeoptValueLocation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeoptFrameState {
    pub state_id: u32,
    pub resume_pc: u32,
    pub parent_state_id: u32,
    pub values: Box<[DeoptValue]>,
}

impl DeoptFrameState {
    pub const NO_PARENT: u32 = u32::MAX;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JitArtifactMetadata {
    pub code_size: u32,
    pub stack_maps: Box<[NativeStackMap]>,
    safepoint_index: Box<[u32]>,
    pub deopt_states: Box<[DeoptFrameState]>,
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
            if roots.windows(2).any(|pair| {
                pair[0].sp_offset.saturating_add(pair[0].kind.width()) > pair[1].sp_offset
            }) {
                return Err(JitError::Internal(format!(
                    "native stack map for {name} contains overlapping root ranges"
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
                roots: roots.into_boxed_slice(),
            });
        }

        Self::try_from_parts(code_size, maps, Vec::new(), name)
    }

    /// Rebuild validated runtime metadata from a persistent AOT manifest.
    pub fn try_from_parts(
        code_size: u32,
        mut maps: Vec<NativeStackMap>,
        deopt_states: Vec<DeoptFrameState>,
        name: &str,
    ) -> Result<Self, JitError> {
        maps.sort_unstable_by_key(|map| map.return_address_offset);
        let mut previous_return_address = None;
        for map in &mut maps {
            if map.return_address_offset > code_size {
                return Err(JitError::Internal(format!(
                    "native stack map for {name} points outside its code: {} > {code_size}",
                    map.return_address_offset
                )));
            }
            if previous_return_address.is_some_and(|previous| previous >= map.return_address_offset)
            {
                return Err(JitError::Internal(format!(
                    "native stack maps for {name} are not strictly ordered"
                )));
            }
            previous_return_address = Some(map.return_address_offset);
            if map.anchor_sp_offset >= map.frame_size {
                return Err(JitError::Internal(format!(
                    "native stack map for {name} has anchor {} outside frame size {}",
                    map.anchor_sp_offset, map.frame_size
                )));
            }
            map.roots.sort_unstable_by_key(|root| root.sp_offset);
            for root in &map.roots {
                if root
                    .sp_offset
                    .checked_add(root.kind.width())
                    .is_none_or(|end| end > map.frame_size)
                {
                    return Err(JitError::Internal(format!(
                        "native stack map for {name} has a root outside frame size {}",
                        map.frame_size
                    )));
                }
            }
            if map.roots.windows(2).any(|pair| {
                pair[0].sp_offset.saturating_add(pair[0].kind.width()) > pair[1].sp_offset
            }) {
                return Err(JitError::Internal(format!(
                    "native stack map for {name} contains overlapping root ranges"
                )));
            }
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

        Self {
            code_size,
            stack_maps: maps.into_boxed_slice(),
            safepoint_index: safepoint_index.into_boxed_slice(),
            deopt_states: Box::new([]),
        }
        .with_deopt_states(deopt_states, name)
    }

    pub(crate) fn with_deopt_states(
        mut self,
        mut states: Vec<DeoptFrameState>,
        name: &str,
    ) -> Result<Self, JitError> {
        states.sort_unstable_by_key(|state| state.state_id);
        for (index, state) in states.iter().enumerate() {
            if index > 0 && states[index - 1].state_id == state.state_id {
                return Err(JitError::Internal(format!(
                    "deopt metadata for {name} contains duplicate state id {}",
                    state.state_id
                )));
            }
            if state.parent_state_id != DeoptFrameState::NO_PARENT
                && states[..index]
                    .binary_search_by_key(&state.parent_state_id, |candidate| candidate.state_id)
                    .is_err()
            {
                return Err(JitError::Internal(format!(
                    "deopt metadata for {name} references absent parent state {}",
                    state.parent_state_id
                )));
            }
            if state
                .values
                .windows(2)
                .any(|pair| pair[0].slot >= pair[1].slot)
            {
                return Err(JitError::Internal(format!(
                    "deopt metadata for {name} has unordered or duplicate slots in state {}",
                    state.state_id
                )));
            }
        }
        self.deopt_states = states.into_boxed_slice();
        Ok(self)
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
            .saturating_add(
                self.deopt_states
                    .len()
                    .saturating_mul(core::mem::size_of::<DeoptFrameState>()),
            )
            .saturating_add(
                self.deopt_states
                    .iter()
                    .map(|state| {
                        state
                            .values
                            .len()
                            .saturating_mul(core::mem::size_of::<DeoptValue>())
                    })
                    .sum::<usize>(),
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

    pub fn deopt_state(&self, state_id: u32) -> Option<&DeoptFrameState> {
        self.deopt_states
            .binary_search_by_key(&state_id, |state| state.state_id)
            .ok()
            .map(|index| &self.deopt_states[index])
    }
}

fn root_kind_for_type(ty: Type) -> Option<NativeRootKind> {
    match ty {
        types::I64 => Some(NativeRootKind::GcRef),
        types::I128 => Some(NativeRootKind::InterfacePair),
        _ => None,
    }
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
                    roots: Box::new([]),
                },
                NativeStackMap {
                    safepoint_id: 1,
                    return_address_offset: 40,
                    frame_size: 48,
                    anchor_sp_offset: 16,
                    roots: Box::new([]),
                },
            ]
            .into_boxed_slice(),
            safepoint_index: vec![0, 1].into_boxed_slice(),
            deopt_states: Box::new([]),
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
                roots: vec![NativeStackRoot {
                    sp_offset: 0,
                    kind: NativeRootKind::GcRef,
                }]
                .into_boxed_slice(),
            }]
            .into_boxed_slice(),
            safepoint_index: vec![0].into_boxed_slice(),
            deopt_states: Box::new([]),
        };

        assert!(metadata.retained_bytes() >= core::mem::size_of::<JitArtifactMetadata>());
        assert!(metadata.retained_bytes() > core::mem::size_of::<NativeStackMap>());
    }

    #[test]
    fn interface_pair_is_retained_as_a_typed_native_root() {
        let metadata = JitArtifactMetadata::from_entries(
            16,
            [(0, 8, 32, vec![(types::I8, 4), (types::I128, 8)])],
            "conditional",
        )
        .expect("conditional stack map");

        let map = metadata.map_for_safepoint_id(0).expect("safepoint");
        assert_eq!(
            map.roots.as_ref(),
            &[NativeStackRoot {
                sp_offset: 8,
                kind: NativeRootKind::InterfacePair,
            }]
        );
    }

    #[test]
    fn deopt_states_are_indexed_and_charged_to_metadata_budget() {
        let base = JitArtifactMetadata::from_entries(8, [], "deopt").expect("base metadata");
        let base_bytes = base.retained_bytes();
        let metadata = base
            .with_deopt_states(
                vec![DeoptFrameState {
                    state_id: 7,
                    resume_pc: 13,
                    parent_state_id: DeoptFrameState::NO_PARENT,
                    values: vec![DeoptValue {
                        slot: 2,
                        kind: DeoptValueKind::GcRef,
                        location: DeoptValueLocation::FiberSlot(2),
                    }]
                    .into_boxed_slice(),
                }],
                "deopt",
            )
            .expect("deopt metadata");

        assert_eq!(
            metadata.deopt_state(7).map(|state| state.resume_pc),
            Some(13)
        );
        assert!(metadata.deopt_state(6).is_none());
        assert!(metadata.retained_bytes() > base_bytes);
    }

    #[test]
    fn deopt_state_rejects_an_absent_parent() {
        let error = JitArtifactMetadata::from_entries(8, [], "deopt-parent")
            .unwrap()
            .with_deopt_states(
                vec![DeoptFrameState {
                    state_id: 1,
                    resume_pc: 0,
                    parent_state_id: 0,
                    values: Box::new([]),
                }],
                "deopt-parent",
            )
            .expect_err("absent parent must fail");
        assert!(error.to_string().contains("absent parent state"));
    }
}
