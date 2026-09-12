//! Immutable logical source chains, separate from executable instruction IDs.
//!
//! A copied source span survives transformations of its original function. A
//! chain entry is attached to an exact instruction in the final function;
//! deleting that instruction deletes the attachment, not its successor's origin.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use super::{DebugInfo, SourceSpan};
use crate::FunctionDef;

/// Shared limits for producers and consumers of logical inline metadata.
pub const MAX_INLINE_SOURCE_DEPTH: usize = 32;
pub const MAX_INLINE_SOURCE_RECORDS: usize = 131_072;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct InlineSourceFrame {
    /// Index of an earlier outer frame, or `NO_PARENT` for the physical caller.
    pub parent: u32,
    pub function_id: u32,
    /// Coordinates captured before rewriting the original function.
    pub span: Option<SourceSpan>,
}

impl InlineSourceFrame {
    pub const NO_PARENT: u32 = u32::MAX;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InlineSourceEntry {
    pub pc: u32,
    pub frame: u32,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InlineFunctionSources {
    pub function_id: u32,
    /// Strictly increasing, exact final instruction PCs.
    pub entries: Vec<InlineSourceEntry>,
}

/// Module-owned sparse tables. Ordinary functions need no additional vector.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InlineSources {
    /// Parent-before-child DAG; identical prefixes may be shared.
    pub frames: Vec<InlineSourceFrame>,
    /// Strictly increasing function IDs.
    pub functions: Vec<InlineFunctionSources>,
}

impl InlineSources {
    pub fn function(&self, function_id: u32) -> Option<&InlineFunctionSources> {
        self.functions
            .binary_search_by_key(&function_id, |entry| entry.function_id)
            .ok()
            .map(|index| &self.functions[index])
    }

    pub fn function_mut(&mut self, function_id: u32) -> Option<&mut InlineFunctionSources> {
        self.functions
            .binary_search_by_key(&function_id, |entry| entry.function_id)
            .ok()
            .map(|index| &mut self.functions[index])
    }

    pub fn frame_at(&self, function_id: u32, pc: u32) -> Option<u32> {
        let entries = &self.function(function_id)?.entries;
        entries
            .binary_search_by_key(&pc, |entry| entry.pc)
            .ok()
            .map(|index| entries[index].frame)
    }

    /// Validate ownership, topology and exact instruction attachments. Bounded
    /// ancestry checks need no scratch allocation, including for untrusted VOBs.
    pub fn validate(
        &self,
        functions: &[FunctionDef],
        file_count: usize,
    ) -> Result<(), &'static str> {
        if self.frames.len() > MAX_INLINE_SOURCE_RECORDS
            || self.functions.len() > functions.len()
            || self.functions.len() > MAX_INLINE_SOURCE_RECORDS
        {
            return Err("inline source table exceeds its record limit");
        }
        for (index, frame) in self.frames.iter().enumerate() {
            if frame.function_id as usize >= functions.len() {
                return Err("inline source frame references a missing function");
            }
            if frame.parent != InlineSourceFrame::NO_PARENT && frame.parent as usize >= index {
                return Err("inline source parent must precede its child");
            }
            if frame.span.is_some_and(|span| {
                span.file_id as usize >= file_count
                    || span.line == 0
                    || span.col == 0
                    || span.len == 0
            }) {
                return Err("inline source frame has invalid source coordinates");
            }
            self.root(index as u32)?;
        }
        let mut count = 0_usize;
        let mut previous_function = None;
        for function in &self.functions {
            if previous_function.is_some_and(|previous| previous >= function.function_id) {
                return Err("inline source functions are not strictly ordered");
            }
            previous_function = Some(function.function_id);
            let Some(definition) = functions.get(function.function_id as usize) else {
                return Err("inline source table references a missing function");
            };
            count = count
                .checked_add(function.entries.len())
                .filter(|count| *count <= MAX_INLINE_SOURCE_RECORDS)
                .ok_or("inline source entries exceed their record limit")?;
            let mut previous_pc = None;
            for entry in &function.entries {
                if entry.pc as usize >= definition.code.len()
                    || previous_pc.is_some_and(|previous| previous >= entry.pc)
                {
                    return Err("inline source entries have invalid instruction PCs");
                }
                previous_pc = Some(entry.pc);
                if self.root(entry.frame)?.function_id != function.function_id {
                    return Err("inline source chain belongs to a different physical function");
                }
            }
        }
        Ok(())
    }

    fn root(&self, mut index: u32) -> Result<&InlineSourceFrame, &'static str> {
        for _ in 0..MAX_INLINE_SOURCE_DEPTH {
            let frame = self
                .frames
                .get(index as usize)
                .ok_or("inline source references a missing frame")?;
            if frame.parent == InlineSourceFrame::NO_PARENT {
                return Ok(frame);
            }
            index = frame.parent;
        }
        Err("inline source chain exceeds its depth limit")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LogicalSourceFrame {
    pub function_id: u32,
    pub span: Option<SourceSpan>,
}

/// Leaf-to-caller iterator over immutable source information. It never walks
/// runtime frames, allocates, or interprets a historical PC as a recovery PC.
pub struct LogicalSourceFrames<'a> {
    sources: &'a InlineSources,
    next: Option<u32>,
    physical: Option<LogicalSourceFrame>,
    remaining: usize,
}

impl<'a> LogicalSourceFrames<'a> {
    pub(super) fn new(debug: &'a DebugInfo, function_id: u32, pc: u32) -> Self {
        let next = debug.inline_sources.frame_at(function_id, pc);
        let physical = next.is_none().then(|| LogicalSourceFrame {
            function_id,
            span: debug.span_at(function_id, pc),
        });
        Self {
            sources: &debug.inline_sources,
            next,
            physical,
            remaining: MAX_INLINE_SOURCE_DEPTH,
        }
    }
}

impl Iterator for LogicalSourceFrames<'_> {
    type Item = LogicalSourceFrame;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(physical) = self.physical.take() {
            return Some(physical);
        }
        if self.remaining == 0 {
            self.next = None;
            return None;
        }
        let index = self.next.take()?;
        let frame = self.sources.frames.get(index as usize)?;
        self.remaining -= 1;
        self.next = (frame.parent != InlineSourceFrame::NO_PARENT).then_some(frame.parent);
        Some(LogicalSourceFrame {
            function_id: frame.function_id,
            span: frame.span,
        })
    }
}

#[cfg(test)]
mod tests;
