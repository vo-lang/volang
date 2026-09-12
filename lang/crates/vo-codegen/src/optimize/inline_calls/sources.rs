//! Bounded interning for source chains while scalar templates are composed.
//! Public records contain copied coordinates; transient instruction PCs never
//! escape into the final module's executable source identities.

use std::collections::HashMap;
use vo_common_core::debug_info::{
    DebugInfo, InlineSourceFrame, InlineSources, MAX_INLINE_SOURCE_DEPTH, MAX_INLINE_SOURCE_RECORDS,
};

pub(super) struct Sources {
    frames: Vec<InlineSourceFrame>,
    depths: Vec<usize>,
    index: HashMap<InlineSourceFrame, u32>,
}

impl Sources {
    pub(super) fn new(sources: &InlineSources) -> Result<Self, String> {
        let mut result = Self {
            frames: Vec::new(),
            depths: Vec::new(),
            index: HashMap::new(),
        };
        if sources.frames.len() > MAX_INLINE_SOURCE_RECORDS {
            return Err("inline source frame budget exceeded".into());
        }
        // Preserve imported indices, including duplicate but valid records.
        for &frame in &sources.frames {
            let depth = result
                .depth_for(frame)
                .ok_or("invalid inline source ancestry")?;
            let id = result.frames.len() as u32;
            result.frames.push(frame);
            result.depths.push(depth);
            result.index.entry(frame).or_insert(id);
        }
        Ok(result)
    }

    fn depth_for(&self, frame: InlineSourceFrame) -> Option<usize> {
        let depth = if frame.parent == InlineSourceFrame::NO_PARENT {
            1
        } else {
            self.depths.get(frame.parent as usize)?.checked_add(1)?
        };
        (depth <= MAX_INLINE_SOURCE_DEPTH).then_some(depth)
    }

    fn intern(&mut self, frame: InlineSourceFrame) -> Option<u32> {
        if let Some(&id) = self.index.get(&frame) {
            return Some(id);
        }
        if self.frames.len() == MAX_INLINE_SOURCE_RECORDS {
            return None;
        }
        let depth = self.depth_for(frame)?;
        let id = self.frames.len() as u32;
        self.frames.push(frame);
        self.depths.push(depth);
        self.index.insert(frame, id);
        Some(id)
    }

    pub(super) fn at(&mut self, debug: &DebugInfo, function_id: u32, pc: u32) -> Option<u32> {
        if let Some(frame) = debug.inline_sources.frame_at(function_id, pc) {
            return Some(frame);
        }
        self.intern(InlineSourceFrame {
            parent: InlineSourceFrame::NO_PARENT,
            function_id,
            span: debug.span_at(function_id, pc),
        })
    }

    /// Add an outer callsite to a complete callee chain, sharing equal prefixes.
    pub(super) fn prepend(&mut self, prefix: u32, mut leaf: u32) -> Option<u32> {
        let prefix_depth = *self.depths.get(prefix as usize)?;
        let leaf_depth = *self.depths.get(leaf as usize)?;
        if prefix_depth + leaf_depth > MAX_INLINE_SOURCE_DEPTH {
            return None;
        }
        let mut chain = [0_u32; MAX_INLINE_SOURCE_DEPTH];
        let mut length = 0;
        loop {
            chain[length] = leaf;
            length += 1;
            let parent = self.frames[leaf as usize].parent;
            if parent == InlineSourceFrame::NO_PARENT {
                break;
            }
            leaf = parent;
        }
        let mut parent = prefix;
        for &id in chain[..length].iter().rev() {
            let mut frame = self.frames[id as usize];
            frame.parent = parent;
            parent = self.intern(frame)?;
        }
        Some(parent)
    }

    pub(super) fn is_inline(&self, frame: u32) -> bool {
        self.frames[frame as usize].parent != InlineSourceFrame::NO_PARENT
    }

    pub(super) fn checkpoint(&self) -> usize {
        self.frames.len()
    }

    /// Failed template/rewrite admission must not consume another candidate's
    /// metadata budget. Existing imported duplicate records stay untouched.
    pub(super) fn rollback(&mut self, checkpoint: usize) {
        for frame in self.frames.drain(checkpoint..) {
            self.index.remove(&frame);
        }
        self.depths.truncate(checkpoint);
    }

    pub(super) fn publish(self, output: &mut InlineSources) {
        let Self {
            frames,
            depths,
            index,
        } = self;
        drop(depths);
        drop(index);
        output.frames = frames;
        compact(output);
    }
}

/// Remove transient template nodes and origins whose instructions disappeared.
/// Parent ordering makes reachability and remapping linear in retained metadata.
pub(super) fn compact(sources: &mut InlineSources) {
    sources
        .functions
        .retain(|function| !function.entries.is_empty());
    let mut live = vec![false; sources.frames.len()];
    for function in &sources.functions {
        for entry in &function.entries {
            live[entry.frame as usize] = true;
        }
    }
    for index in (0..sources.frames.len()).rev() {
        let parent = sources.frames[index].parent;
        if live[index] && parent != InlineSourceFrame::NO_PARENT {
            live[parent as usize] = true;
        }
    }
    let mut remap = vec![InlineSourceFrame::NO_PARENT; sources.frames.len()];
    let mut next = 0_u32;
    for (index, &keep) in live.iter().enumerate() {
        if keep {
            remap[index] = next;
            next += 1;
        }
    }
    let mut index = 0;
    sources.frames.retain_mut(|frame| {
        let keep = live[index];
        index += 1;
        if keep && frame.parent != InlineSourceFrame::NO_PARENT {
            frame.parent = remap[frame.parent as usize];
        }
        keep
    });
    for function in &mut sources.functions {
        for entry in &mut function.entries {
            entry.frame = remap[entry.frame as usize];
        }
        function.entries.shrink_to_fit();
    }
    sources.frames.shrink_to_fit();
    sources.functions.shrink_to_fit();
}
