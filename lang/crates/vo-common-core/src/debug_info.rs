//! Debug information for runtime error reporting.
//!
//! This module provides structures to map bytecode positions (func_id, pc)
//! back to source code locations (file, span).

#[cfg(not(feature = "std"))]
use alloc::{
    string::{String, ToString},
    vec::Vec,
};
use core::fmt;

mod inline;
pub use inline::{
    InlineFunctionSources, InlineSourceEntry, InlineSourceFrame, InlineSources, LogicalSourceFrame,
    LogicalSourceFrames, MAX_INLINE_SOURCE_DEPTH, MAX_INLINE_SOURCE_RECORDS,
};

/// Stable source coordinates, independent of any transformed bytecode PC.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SourceSpan {
    pub file_id: u32,
    pub line: u32,
    pub col: u32,
    pub len: u32,
}

/// Compact module-local instruction identity. The high word contains the
/// function index plus one; the low word contains its bytecode PC. Zero and
/// encodings with a zero high word have no instruction identity.
///
/// This is independent of optional source-file debug data and of the physical
/// PC used to restore generated frames. Consumers validate it against the
/// owning immutable module before resolving a source location.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InstructionSource(core::num::NonZeroU64);

impl InstructionSource {
    pub const fn from_parts(func_id: u32, pc: u32) -> Option<Self> {
        if func_id == u32::MAX {
            return None;
        }
        Self::from_raw(((func_id as u64 + 1) << 32) | pc as u64)
    }

    pub const fn from_raw(raw: u64) -> Option<Self> {
        if raw >> 32 == 0 {
            return None;
        }
        match core::num::NonZeroU64::new(raw) {
            Some(raw) => Some(Self(raw)),
            None => None,
        }
    }

    pub const fn raw(self) -> u64 {
        self.0.get()
    }
    pub const fn func_id(self) -> u32 {
        (self.raw() >> 32) as u32 - 1
    }
    pub const fn pc(self) -> u32 {
        self.raw() as u32
    }
}

/// Source anchors for one observed failure. Native inlining currently admits
/// trapping heap-reading leaves and total scalar chains through separate routes;
/// retaining the leaf plus its physical callsite covers those observable frames.
/// Each anchor resolves through the shared immutable bytecode source DAG.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DiagnosticSource {
    instruction: InstructionSource,
    inlined_in: Option<InstructionSource>,
}

impl DiagnosticSource {
    pub fn new(function_id: u32, pc: u32) -> Option<Self> {
        InstructionSource::from_parts(function_id, pc)
            .map(|instruction| Self::from_instruction(instruction, None))
    }

    pub fn from_instruction(
        instruction: InstructionSource,
        inlined_in: Option<InstructionSource>,
    ) -> Self {
        Self {
            instruction,
            inlined_in: inlined_in.filter(|parent| *parent != instruction),
        }
    }

    pub const fn instruction(self) -> InstructionSource {
        self.instruction
    }
    pub const fn inlined_in(self) -> Option<InstructionSource> {
        self.inlined_in
    }

    /// No allocation or runtime-frame traversal. Each component is bounded by
    /// MAX_INLINE_SOURCE_DEPTH, including reads of unverified diagnostic data.
    pub fn logical_frames(
        self,
        debug: &DebugInfo,
    ) -> impl Iterator<Item = LogicalSourceFrame> + '_ {
        debug
            .logical_frames(self.instruction.func_id(), self.instruction.pc())
            .chain(
                self.inlined_in
                    .into_iter()
                    .flat_map(move |parent| debug.logical_frames(parent.func_id(), parent.pc())),
            )
    }
}

/// Single debug location entry.
/// Stores line:col:len for error display and highlighting.
#[derive(Clone, Copy, Debug)]
pub struct DebugLoc {
    pub pc: u32,
    pub file_id: u32,
    /// Line number (1-indexed)
    pub line: u32,
    /// Column number (1-indexed)
    pub col: u32,
    /// Length of the span (for highlighting)
    pub len: u32,
}

/// Function-level debug information.
#[derive(Clone, Debug, Default)]
pub struct FuncDebugInfo {
    pub entries: Vec<DebugLoc>,
}

impl FuncDebugInfo {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn add(&mut self, pc: u32, file_id: u32, line: u32, col: u32, len: u32) {
        self.entries.push(DebugLoc {
            pc,
            file_id,
            line,
            col,
            len,
        });
    }

    pub fn sort(&mut self) {
        self.entries.sort_by_key(|e| e.pc);
    }
}

/// Module-level debug information.
#[derive(Clone, Debug, Default)]
pub struct DebugInfo {
    pub files: Vec<String>,
    pub funcs: Vec<FuncDebugInfo>,
    pub inline_sources: InlineSources,
}

/// Source location result from lookup.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceLoc {
    pub file: String,
    /// Line number (1-indexed)
    pub line: u32,
    /// Column number (1-indexed)
    pub col: u32,
    /// Length of the span (for highlighting)
    pub len: u32,
}

/// Owned diagnostic frame, retained after its executable module is released.
/// Missing optional debug data remains explicit; function identity is preserved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ResolvedSourceFrame {
    pub function_id: u32,
    pub function_name: Option<String>,
    pub location: Option<SourceLoc>,
}

impl fmt::Display for ResolvedSourceFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.function_name {
            f.write_str(name)?;
        } else {
            write!(f, "function #{}", self.function_id)?;
        }
        if let Some(location) = &self.location {
            write!(f, " ({location})")?;
        }
        Ok(())
    }
}

impl SourceLoc {
    pub fn new(file: impl Into<String>, line: u32, col: u32, len: u32) -> Self {
        Self {
            file: file.into(),
            line,
            col,
            len,
        }
    }
}

impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}

impl DebugInfo {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get or create file ID for a file path.
    pub fn get_or_add_file(&mut self, file: &str) -> u32 {
        if let Some(idx) = self.files.iter().position(|f| f == file) {
            u32::try_from(idx).expect("debug file index exceeds u32::MAX")
        } else {
            let idx = u32::try_from(self.files.len()).expect("debug file count exceeds u32::MAX");
            self.files.push(file.to_string());
            idx
        }
    }

    /// Ensure funcs vec has entry for func_id.
    pub fn ensure_func(&mut self, func_id: u32) {
        while self.funcs.len() <= func_id as usize {
            self.funcs.push(FuncDebugInfo::new());
        }
    }

    /// Add a debug location for a function.
    pub fn add_loc(&mut self, func_id: u32, pc: u32, file: &str, line: u32, col: u32, len: u32) {
        let file_id = self.get_or_add_file(file);
        self.ensure_func(func_id);
        self.funcs[func_id as usize].add(pc, file_id, line, col, len);
    }

    /// Sort all function entries by PC.
    pub fn finalize(&mut self) {
        for func in &mut self.funcs {
            func.sort();
        }
    }

    /// Physical source interval, before considering logical inline ancestry.
    pub fn span_at(&self, func_id: u32, pc: u32) -> Option<SourceSpan> {
        let func = self.funcs.get(func_id as usize)?;
        if func.entries.is_empty() {
            return None;
        }
        // Binary search for the largest pc <= target
        let idx = func.entries.partition_point(|e| e.pc <= pc);
        if idx == 0 {
            return None;
        }
        let entry = &func.entries[idx - 1];
        Some(SourceSpan {
            file_id: entry.file_id,
            line: entry.line,
            col: entry.col,
            len: entry.len,
        })
    }

    pub fn logical_frames(&self, func_id: u32, pc: u32) -> LogicalSourceFrames<'_> {
        LogicalSourceFrames::new(self, func_id, pc)
    }

    /// Resolve one already bounded logical frame for a cold diagnostic path.
    pub fn resolve_frame(
        &self,
        frame: LogicalSourceFrame,
        functions: &[crate::bytecode::FunctionDef],
    ) -> ResolvedSourceFrame {
        ResolvedSourceFrame {
            function_id: frame.function_id,
            function_name: functions
                .get(frame.function_id as usize)
                .map(|function| &function.name)
                .filter(|name| !name.is_empty())
                .cloned(),
            location: frame.span.and_then(|span| self.resolve_span(span)),
        }
    }

    pub fn resolve_span(&self, span: SourceSpan) -> Option<SourceLoc> {
        Some(SourceLoc {
            file: self.files.get(span.file_id as usize)?.clone(),
            line: span.line,
            col: span.col,
            len: span.len,
        })
    }

    /// Resolve an exact inline origin, otherwise the physical source interval
    /// at or before `pc`. Inline entries never leak onto neighboring instructions.
    pub fn lookup(&self, func_id: u32, pc: u32) -> Option<SourceLoc> {
        self.resolve_span(self.logical_frames(func_id, pc).next()?.span?)
    }
}

#[cfg(test)]
mod instruction_source_tests {
    use super::InstructionSource;

    #[test]
    fn compact_instruction_sources_preserve_zero_pc_and_full_width_coordinates() {
        for function in [0, 7, u32::MAX - 1] {
            for pc in [0, 23, u32::MAX] {
                let source = InstructionSource::from_parts(function, pc).unwrap();
                assert_eq!(InstructionSource::from_raw(source.raw()), Some(source));
                assert_eq!((source.func_id(), source.pc()), (function, pc));
            }
        }
        assert_eq!(InstructionSource::from_parts(u32::MAX, 0), None);
        for raw in [0, 1, u32::MAX as u64] {
            assert_eq!(InstructionSource::from_raw(raw), None);
        }
    }
}
