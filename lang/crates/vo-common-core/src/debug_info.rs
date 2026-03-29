//! Debug information for runtime error reporting.
//!
//! This module provides structures to map bytecode positions (func_id, pc)
//! back to source code locations (file, span).

use std::fmt;

/// Single debug location entry.
/// Stores line:col:len for error display and highlighting.
#[derive(Clone, Copy, Debug)]
pub struct DebugLoc {
    pub pc: u32,
    pub file_id: u16,
    /// Line number (1-indexed)
    pub line: u32,
    /// Column number (1-indexed)
    pub col: u16,
    /// Length of the span (for highlighting)
    pub len: u16,
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

    pub fn add(&mut self, pc: u32, file_id: u16, line: u32, col: u16, len: u16) {
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
}

/// Source location result from lookup.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceLoc {
    pub file: String,
    /// Line number (1-indexed)
    pub line: u32,
    /// Column number (1-indexed)
    pub col: u16,
    /// Length of the span (for highlighting)
    pub len: u16,
}

impl SourceLoc {
    pub fn new(file: impl Into<String>, line: u32, col: u16, len: u16) -> Self {
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
        Self {
            files: Vec::new(),
            funcs: Vec::new(),
        }
    }

    /// Get or create file ID for a file path.
    pub fn get_or_add_file(&mut self, file: &str) -> u16 {
        if let Some(idx) = self.files.iter().position(|f| f == file) {
            idx as u16
        } else {
            let idx = self.files.len() as u16;
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
    pub fn add_loc(&mut self, func_id: u32, pc: u32, file: &str, line: u32, col: u16, len: u16) {
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

    /// Lookup source location for (func_id, pc).
    /// Returns the location of the instruction at or before pc.
    pub fn lookup(&self, func_id: u32, pc: u32) -> Option<SourceLoc> {
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
        let file = self.files.get(entry.file_id as usize)?;
        Some(SourceLoc {
            file: file.clone(),
            line: entry.line,
            col: entry.col,
            len: entry.len,
        })
    }
}
