//! One unpublished transfer packet owns its graph and traversal workspace.
use super::{
    pack_value_with_workspace, PackObjectGraph, PackOutputError, PackTypeContext, PackWorkspace,
    PackedValue,
};
use crate::gc::Gc;
use crate::ValueMeta;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

/// Metadata and graph identity cannot outlive the source heap or move between
/// programs. Only owned wire bytes leave the encoder when it finishes.
pub(crate) struct PacketEncoder<'a> {
    gc: &'a Gc,
    context: PackTypeContext<'a>,
    packed: PackedValue,
    graph: PackObjectGraph,
    workspace: PackWorkspace,
}

impl<'a> PacketEncoder<'a> {
    pub(crate) fn new(
        gc: &'a Gc,
        context: PackTypeContext<'a>,
        prefix: Vec<u8>,
        limit: usize,
    ) -> Result<Self, PackOutputError> {
        if prefix.len() > limit {
            return Err(PackOutputError::LengthOverflow {
                limit,
                attempted: prefix.len(),
            });
        }
        Ok(Self {
            gc,
            context,
            packed: PackedValue {
                data: prefix,
                output_limit: limit,
                output_error: None,
            },
            graph: PackObjectGraph::default(),
            workspace: PackWorkspace::default(),
        })
    }

    pub(crate) fn extend(&mut self, bytes: &[u8]) -> Result<(), PackOutputError> {
        self.packed.extend_encoded(bytes);
        self.packed.output_error.map_or(Ok(()), Err)
    }

    /// Append one u32-length-prefixed value without a temporary encoded buffer.
    /// Failure poisons the unpublished packet, including its graph identity.
    ///
    /// # Safety
    /// The source slots, embedded rooted references and canonical metadata must
    /// satisfy the same contract as `try_pack_slots_with_named_type_metas`.
    pub(crate) unsafe fn append_chunk(
        &mut self,
        src: &[u64],
        meta: ValueMeta,
    ) -> Result<(), PackOutputError> {
        if let Some(error) = self.packed.output_error {
            return Err(error);
        }
        let prefix = self.packed.data.len();
        self.extend(&[0; 4])?;
        let payload = self.packed.data.len();
        let packet_limit = self.packed.output_limit;
        self.packed.output_limit = packet_limit.min(payload.saturating_add(u32::MAX as usize));
        unsafe {
            pack_value_with_workspace(
                &mut self.packed,
                self.gc,
                src,
                meta,
                self.context,
                &mut self.graph,
                &mut self.workspace,
            );
        }
        self.packed.output_limit = packet_limit;
        if let Some(error) = self.packed.output_error {
            return Err(error);
        }
        let length = u32::try_from(self.packed.data.len() - payload)
            .expect("chunk output is bounded to its wire length");
        self.packed.data[prefix..payload].copy_from_slice(&length.to_le_bytes());
        Ok(())
    }

    pub(crate) fn finish(self) -> Result<Vec<u8>, PackOutputError> {
        self.packed.finish_output().map(PackedValue::into_data)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pack::pack_slots_with_named_type_metas_and_cache_limited;
    use crate::{RuntimeType, ValueKind, ValueRttid};

    #[test]
    fn packet_bytes_match_independent_chunks_and_reuse_array_layouts() {
        let gc = Gc::new();
        let types = [
            RuntimeType::Basic(ValueKind::Int64),
            RuntimeType::Array {
                len: 4,
                elem: ValueRttid::new(0, ValueKind::Int64),
            },
        ];
        let context = PackTypeContext::new(&[], &types);
        let meta = ValueMeta::new(1, ValueKind::Array);
        let slots = [1, 2, 3, 4];
        let mut encoder = PacketEncoder::new(&gc, context, vec![7, 9], 4096).unwrap();
        let mut expected = vec![7, 9];
        let mut graph = PackObjectGraph::default();
        for _ in 0..32 {
            let old = unsafe {
                pack_slots_with_named_type_metas_and_cache_limited(
                    &gc,
                    &slots,
                    meta,
                    &[],
                    &[],
                    &types,
                    &mut graph,
                    4096,
                )
            }
            .unwrap();
            expected.extend_from_slice(&(old.data().len() as u32).to_le_bytes());
            expected.extend_from_slice(old.data());
            unsafe { encoder.append_chunk(&slots, meta) }.unwrap();
        }
        assert_eq!(encoder.workspace.layouts.array_layout_resolutions, 1);
        assert!(encoder.workspace.layouts.slot_count_resolutions <= 2);
        assert!(encoder.workspace.tasks.is_empty());
        assert_eq!(encoder.finish().unwrap(), expected);
    }

    #[test]
    fn packet_limit_is_sticky_and_failed_traversal_releases_borrowed_tasks() {
        let gc = Gc::new();
        let types = [
            RuntimeType::Basic(ValueKind::Int64),
            RuntimeType::Array {
                len: 4,
                elem: ValueRttid::new(0, ValueKind::Int64),
            },
        ];
        let context = PackTypeContext::new(&[], &types);
        let mut encoder = PacketEncoder::new(&gc, context, vec![7, 9], 17).unwrap();
        let error =
            unsafe { encoder.append_chunk(&[1, 2, 3, 4], ValueMeta::new(1, ValueKind::Array)) }
                .unwrap_err();
        assert!(matches!(
            error,
            PackOutputError::LengthOverflow { limit: 17, .. }
        ));
        assert!(encoder.workspace.tasks.is_empty());
        let bytes = encoder.packed.data.clone();
        assert_eq!(encoder.extend(&[5]), Err(error));
        assert_eq!(
            unsafe { encoder.append_chunk(&[9], ValueMeta::new(0, ValueKind::Int64)) },
            Err(error)
        );
        assert_eq!(encoder.packed.data, bytes);
        assert_eq!(encoder.finish(), Err(error));
    }
    #[test]
    fn cache_saturation_keeps_new_array_shapes_encodable() {
        use super::super::RuntimeLayoutCache;
        let gc = Gc::new();
        let count = RuntimeLayoutCache::MAX_ENTRIES + 3;
        let mut types = vec![RuntimeType::Basic(ValueKind::Int64)];
        types.extend((0..count).map(|_| RuntimeType::Array {
            len: 1,
            elem: ValueRttid::new(0, ValueKind::Int64),
        }));
        let context = PackTypeContext::new(&[], &types);
        let mut encoder = PacketEncoder::new(&gc, context, Vec::new(), 1 << 20).unwrap();
        let mut expected = Vec::new();
        let mut graph = PackObjectGraph::default();
        // The final shape is visited again after both caches have saturated.
        for id in (1..=count).chain([count, 1, count]) {
            let meta = ValueMeta::new(id as u32, ValueKind::Array);
            let slots = [id as u64];
            let old = unsafe {
                pack_slots_with_named_type_metas_and_cache_limited(
                    &gc,
                    &slots,
                    meta,
                    &[],
                    &[],
                    &types,
                    &mut graph,
                    4096,
                )
            }
            .unwrap();
            expected.extend_from_slice(&(old.data().len() as u32).to_le_bytes());
            expected.extend_from_slice(old.data());
            unsafe { encoder.append_chunk(&slots, meta) }.unwrap();
        }
        assert_eq!(
            encoder.workspace.layouts.slot_counts.len(),
            RuntimeLayoutCache::MAX_ENTRIES
        );
        assert_eq!(
            encoder.workspace.layouts.array_layouts.len(),
            RuntimeLayoutCache::MAX_ENTRIES
        );
        assert!(encoder.workspace.tasks.is_empty());
        assert_eq!(encoder.finish().unwrap(), expected);
    }
}
