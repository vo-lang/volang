//! Source file management with global position space.
//!
//! This module provides types for managing source files using a global position space.
//! Each file is assigned a base offset, so any `BytePos` or `Span` uniquely identifies
//! both the file and the location within it.
//!
//! This design is similar to rustc's source mapping:
//! - File 1: byte boundaries [0, 1000]
//! - File 2: byte boundaries [1001, 2500]
//! - File 3: byte boundaries [2501, 3000]
//!
//! The inclusive final boundary is the file's EOF position. Consecutive files
//! remain disjoint because the next base is one greater than the previous EOF.
//!
//! Benefits:
//! - No need to pass `FileId` alongside `Span` everywhere
//! - Simplified diagnostic APIs
//! - AST node spans contain complete location information

use std::fmt;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::span::{BytePos, Span};
use vo_common_core::SourceProvider;

/// Failure to represent a file in the source map's 32-bit position space.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SourceMapError {
    /// The next file ID would collide with [`FileId::DUMMY`].
    FileIdExhausted { file_count: usize },
    /// A single source is too large for 32-bit offsets and line numbers.
    SourceTooLarge { bytes: usize },
    /// The remaining global position space cannot hold the source and its gap.
    PositionSpaceExhausted { base: u32, bytes: usize },
    /// A real source file was constructed with the reserved dummy ID.
    DummyFileId,
}

impl fmt::Display for SourceMapError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FileIdExhausted { file_count } => {
                write!(f, "source file ID space exhausted after {file_count} files")
            }
            Self::SourceTooLarge { bytes } => {
                write!(
                    f,
                    "source file is too large for 32-bit source metadata ({bytes} bytes)"
                )
            }
            Self::PositionSpaceExhausted { base, bytes } => write!(
                f,
                "source position space exhausted at byte {base} while adding {bytes} bytes"
            ),
            Self::DummyFileId => write!(f, "the dummy file ID is reserved for unknown sources"),
        }
    }
}

impl std::error::Error for SourceMapError {}

/// A unique identifier for a source file within a `SourceMap`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

impl FileId {
    /// Creates a new file ID.
    #[inline]
    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    /// Returns the raw ID value.
    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0
    }

    /// A dummy file ID for generated code or unknown sources.
    pub const DUMMY: FileId = FileId(u32::MAX);

    /// Returns true if this is a dummy file ID.
    #[inline]
    pub const fn is_dummy(self) -> bool {
        self.0 == u32::MAX
    }
}

impl fmt::Debug for FileId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_dummy() {
            write!(f, "FileId(DUMMY)")
        } else {
            write!(f, "FileId({})", self.0)
        }
    }
}

impl fmt::Display for FileId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Line and column information for a position in a source file.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LineCol {
    /// 1-indexed line number.
    pub line: u32,
    /// 1-indexed column number (in UTF-8 bytes).
    pub column: u32,
}

impl LineCol {
    /// Creates a new line/column position.
    #[inline]
    pub const fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

impl fmt::Display for LineCol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// Complete source location with file, line, and column.
/// Used by both compile-time and runtime error reporting.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceLoc {
    pub file: String,
    pub line: u32,
    pub col: u32,
}

impl SourceLoc {
    pub fn new(file: String, line: u32, col: u32) -> Self {
        Self { file, line, col }
    }
}

impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}

/// A source file with its content and metadata.
#[derive(Clone)]
pub struct SourceFile {
    /// The file ID.
    id: FileId,
    /// The file name or path.
    name: Arc<str>,
    /// The absolute path to the file, if available.
    path: Option<PathBuf>,
    /// The source text content.
    source: Arc<str>,
    /// Base offset in global position space.
    base: u32,
    /// Byte offsets of line starts (0-indexed, relative to base).
    /// The first element is always 0.
    line_starts: Vec<u32>,
}

impl SourceFile {
    /// Creates a new source file with a base offset.
    ///
    /// # Panics
    ///
    /// Panics when the file cannot be represented by 32-bit source positions.
    pub fn new(
        id: FileId,
        name: impl Into<Arc<str>>,
        source: impl Into<Arc<str>>,
        base: u32,
    ) -> Self {
        Self::try_new(id, name, source, base)
            .expect("source file must fit in the 32-bit source position space")
    }

    /// Tries to create a source file with a base offset.
    pub fn try_new(
        id: FileId,
        name: impl Into<Arc<str>>,
        source: impl Into<Arc<str>>,
        base: u32,
    ) -> Result<Self, SourceMapError> {
        if id.is_dummy() {
            return Err(SourceMapError::DummyFileId);
        }

        let name = name.into();
        let source = source.into();
        checked_source_end(base, source.len())?;
        let line_starts = Self::compute_line_starts(&source);

        Ok(Self {
            id,
            name,
            path: None,
            source,
            base,
            line_starts,
        })
    }

    /// Creates a new source file with a path.
    ///
    /// # Panics
    ///
    /// Panics when the file cannot be represented by 32-bit source positions.
    pub fn with_path(
        id: FileId,
        name: impl Into<Arc<str>>,
        path: impl Into<PathBuf>,
        source: impl Into<Arc<str>>,
        base: u32,
    ) -> Self {
        Self::try_with_path(id, name, path, source, base)
            .expect("source file must fit in the 32-bit source position space")
    }

    /// Tries to create a source file with a path.
    pub fn try_with_path(
        id: FileId,
        name: impl Into<Arc<str>>,
        path: impl Into<PathBuf>,
        source: impl Into<Arc<str>>,
        base: u32,
    ) -> Result<Self, SourceMapError> {
        let mut file = Self::try_new(id, name, source, base)?;
        file.path = Some(path.into());
        Ok(file)
    }

    /// Computes the byte offsets of line starts.
    fn compute_line_starts(source: &str) -> Vec<u32> {
        let mut starts = vec![0];
        for (i, c) in source.char_indices() {
            if c == '\n' {
                starts.push((i + 1) as u32);
            }
        }
        starts
    }

    /// Returns the file ID.
    #[inline]
    pub const fn id(&self) -> FileId {
        self.id
    }

    /// Returns the file name.
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the file path, if available.
    #[inline]
    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }

    /// Returns the source text.
    #[inline]
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Returns the base offset in global position space.
    #[inline]
    pub const fn base(&self) -> u32 {
        self.base
    }

    /// Returns the end position (exclusive) in global position space.
    #[inline]
    pub fn end_pos(&self) -> u32 {
        self.base + self.source.len() as u32
    }

    /// Returns the length of the source in bytes.
    #[inline]
    pub fn len(&self) -> usize {
        self.source.len()
    }

    /// Returns true if the source is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.source.is_empty()
    }

    /// Returns the number of lines in the file.
    #[inline]
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    /// Returns the byte offset of the start of the given line (0-indexed).
    #[inline]
    pub fn line_start(&self, line: usize) -> Option<u32> {
        self.line_starts.get(line).copied()
    }

    /// Returns the byte offset of the end of the given line (0-indexed).
    /// This is the offset of the newline character or end of file.
    pub fn line_end(&self, line: usize) -> Option<u32> {
        if line >= self.line_starts.len() {
            return None;
        }

        if line + 1 < self.line_starts.len() {
            // End is start of next line minus the newline character
            Some(self.line_starts[line + 1] - 1)
        } else {
            // Last line ends at end of file
            Some(self.source.len() as u32)
        }
    }

    /// Returns the content of the given line (0-indexed), without the trailing newline.
    pub fn line_content(&self, line: usize) -> Option<&str> {
        let start = self.line_start(line)? as usize;
        let end = self.line_end(line)? as usize;
        Some(&self.source[start..end])
    }

    /// Returns true if the global position is within this file, including EOF.
    #[inline]
    pub fn contains_pos(&self, pos: BytePos) -> bool {
        let p = pos.to_u32();
        p >= self.base && p <= self.end_pos()
    }

    /// Returns true if the given span is within this file.
    #[inline]
    pub fn contains_span(&self, span: Span) -> bool {
        span.is_well_formed()
            && self.contains_pos(span.start)
            && span.end.to_u32() <= self.end_pos()
    }

    /// Converts a global byte position to local offset within this file.
    ///
    /// # Panics
    ///
    /// Panics when `pos` is outside this file's inclusive byte-boundary range.
    #[inline]
    pub fn local_offset(&self, pos: BytePos) -> u32 {
        self.try_local_offset(pos)
            .expect("source position must belong to the source file")
    }

    /// Tries to convert a global byte position to a local offset.
    #[inline]
    pub fn try_local_offset(&self, pos: BytePos) -> Option<u32> {
        self.contains_pos(pos).then(|| pos.to_u32() - self.base)
    }

    /// Converts a local offset to global byte position.
    ///
    /// # Panics
    ///
    /// Panics when `local` is past this file's EOF boundary.
    #[inline]
    pub fn global_pos(&self, local: u32) -> BytePos {
        self.try_global_pos(local)
            .expect("local source offset must not exceed the source length")
    }

    /// Tries to convert a local offset to a global byte position.
    #[inline]
    pub fn try_global_pos(&self, local: u32) -> Option<BytePos> {
        if local as usize > self.source.len() {
            return None;
        }
        self.base.checked_add(local).map(BytePos::new)
    }

    /// Converts a global byte position to line/column.
    pub fn line_col(&self, pos: BytePos) -> LineCol {
        let offset = self.local_offset(pos);

        // Binary search for the line containing this offset
        let line = match self.line_starts.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line.saturating_sub(1),
        };

        let line_start = self.line_starts[line];
        let column = offset.saturating_sub(line_start) + 1;

        LineCol {
            line: (line + 1) as u32,
            column,
        }
    }

    /// Converts a span to start and end line/column positions.
    ///
    /// # Panics
    ///
    /// Panics when the complete span does not belong to this file.
    pub fn span_line_col(&self, span: Span) -> (LineCol, LineCol) {
        self.try_span_line_col(span)
            .expect("source span must belong to the source file")
    }

    /// Tries to convert a complete span to start and end line/column positions.
    pub fn try_span_line_col(&self, span: Span) -> Option<(LineCol, LineCol)> {
        self.contains_span(span)
            .then(|| (self.line_col(span.start), self.line_col(span.end)))
    }

    /// Returns the source text for a given span.
    ///
    /// # Panics
    ///
    /// Panics when the span is outside this file or splits a UTF-8 code point.
    pub fn span_text(&self, span: Span) -> &str {
        self.try_span_text(span)
            .expect("source span must be in bounds and on UTF-8 boundaries")
    }

    /// Tries to return the source text for a given span.
    pub fn try_span_text(&self, span: Span) -> Option<&str> {
        if !self.contains_span(span) {
            return None;
        }
        let start = self.try_local_offset(span.start)? as usize;
        let end = self.try_local_offset(span.end)? as usize;
        self.source.get(start..end)
    }

    /// Returns a span covering the entire file.
    pub fn full_span(&self) -> Span {
        Span::from_u32(self.base, self.end_pos())
    }
}

impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceFile")
            .field("id", &self.id)
            .field("name", &self.name)
            .field("base", &self.base)
            .field("len", &self.source.len())
            .field("lines", &self.line_starts.len())
            .finish()
    }
}

/// A central registry for all source files with global position space.
///
/// The `SourceMap` owns all source files and provides efficient lookup
/// from any global position to its containing file.
#[derive(Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
    /// The next available base offset.
    next_base: u32,
}

impl SourceMap {
    /// Creates a new empty source map.
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            next_base: 0,
        }
    }

    /// Adds a source file to the map and returns its ID.
    /// The file's base offset is automatically assigned.
    ///
    /// # Panics
    ///
    /// Panics when the source map's 32-bit ID or position space is exhausted.
    pub fn add_file(&mut self, name: impl Into<Arc<str>>, source: impl Into<Arc<str>>) -> FileId {
        self.try_add_file(name, source)
            .expect("source file must fit in the source map")
    }

    /// Tries to add a source file to the map and returns its ID.
    pub fn try_add_file(
        &mut self,
        name: impl Into<Arc<str>>,
        source: impl Into<Arc<str>>,
    ) -> Result<FileId, SourceMapError> {
        let source = source.into();
        let (id, base, next_base) =
            checked_source_allocation(self.files.len(), self.next_base, source.len())?;

        let file = SourceFile::try_new(id, name, source, base)?;
        self.files.push(file);
        self.next_base = next_base;
        Ok(id)
    }

    /// Adds a source file with a path to the map and returns its ID.
    ///
    /// # Panics
    ///
    /// Panics when the source map's 32-bit ID or position space is exhausted.
    pub fn add_file_with_path(
        &mut self,
        name: impl Into<Arc<str>>,
        path: impl Into<PathBuf>,
        source: impl Into<Arc<str>>,
    ) -> FileId {
        self.try_add_file_with_path(name, path, source)
            .expect("source file must fit in the source map")
    }

    /// Tries to add a source file with a path to the map and returns its ID.
    pub fn try_add_file_with_path(
        &mut self,
        name: impl Into<Arc<str>>,
        path: impl Into<PathBuf>,
        source: impl Into<Arc<str>>,
    ) -> Result<FileId, SourceMapError> {
        let source = source.into();
        let (id, base, next_base) =
            checked_source_allocation(self.files.len(), self.next_base, source.len())?;

        let file = SourceFile::try_with_path(id, name, path, source, base)?;
        self.files.push(file);
        self.next_base = next_base;
        Ok(id)
    }

    /// Loads a file from disk and adds it to the map.
    pub fn load_file(&mut self, path: impl AsRef<Path>) -> std::io::Result<FileId> {
        let path = path.as_ref();
        let source = crate::vfs::read_text_file(path)?;
        let name = path
            .file_name()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| path.to_string_lossy().into_owned());

        self.try_add_file_with_path(name, path.to_path_buf(), source)
            .map_err(|error| std::io::Error::new(std::io::ErrorKind::InvalidData, error))
    }

    /// Returns the source file for the given ID.
    pub fn get_file(&self, id: FileId) -> Option<&SourceFile> {
        if id.is_dummy() {
            return None;
        }
        self.files.get(id.0 as usize)
    }

    /// Looks up the file containing the given global position.
    pub fn lookup_file(&self, pos: BytePos) -> Option<&SourceFile> {
        let p = pos.to_u32();
        // Binary search for the file
        let idx = self.files.partition_point(|f| f.base <= p);
        if idx > 0 {
            let file = &self.files[idx - 1];
            if file.contains_pos(pos) {
                return Some(file);
            }
        }
        None
    }

    /// Looks up the file containing the given span.
    pub fn lookup_span(&self, span: Span) -> Option<&SourceFile> {
        if !span.is_well_formed() {
            return None;
        }
        let file = self.lookup_file(span.start)?;
        file.contains_span(span).then_some(file)
    }

    /// Returns the FileId for a span (by looking up the containing file).
    pub fn span_file_id(&self, span: Span) -> Option<FileId> {
        self.lookup_span(span).map(|f| f.id())
    }

    /// Returns the number of files in the map.
    #[inline]
    pub fn file_count(&self) -> usize {
        self.files.len()
    }

    /// Returns an iterator over all files.
    pub fn files(&self) -> impl Iterator<Item = &SourceFile> {
        self.files.iter()
    }

    /// Returns the source text for a file.
    pub fn source(&self, id: FileId) -> Option<&str> {
        self.get_file(id).map(|f| f.source())
    }

    /// Returns the file name for a file.
    pub fn file_name(&self, id: FileId) -> Option<&str> {
        self.get_file(id).map(|f| f.name())
    }

    /// Converts a global position to line/column.
    pub fn line_col(&self, pos: BytePos) -> Option<LineCol> {
        self.lookup_file(pos).map(|f| f.line_col(pos))
    }

    /// Returns the source text for a span.
    pub fn span_text(&self, span: Span) -> Option<&str> {
        self.lookup_span(span)
            .and_then(|file| file.try_span_text(span))
    }

    /// Returns the base offset for a file (for use in lexer/parser).
    pub fn file_base(&self, id: FileId) -> Option<u32> {
        self.get_file(id).map(|f| f.base())
    }

    /// Returns formatted location string: "filename:line:col"
    pub fn format_pos(&self, pos: BytePos) -> String {
        if let Some(file) = self.lookup_file(pos) {
            let lc = file.line_col(pos);
            format!("{}:{}:{}", file.name(), lc.line, lc.column)
        } else {
            format!("?:{}", pos.to_u32())
        }
    }

    /// Returns formatted location string for a span: "filename:line:col"
    pub fn format_span(&self, span: Span) -> String {
        let Some(file) = self.lookup_span(span) else {
            return "?:unknown".to_string();
        };
        let lc = file.line_col(span.start);
        format!("{}:{}:{}", file.name(), lc.line, lc.column)
    }
}

fn checked_source_end(base: u32, source_len: usize) -> Result<u32, SourceMapError> {
    if source_len >= u32::MAX as usize {
        return Err(SourceMapError::SourceTooLarge { bytes: source_len });
    }
    let len = source_len as u32;
    base.checked_add(len)
        .ok_or(SourceMapError::PositionSpaceExhausted {
            base,
            bytes: source_len,
        })
}

fn checked_source_allocation(
    file_count: usize,
    next_base: u32,
    source_len: usize,
) -> Result<(FileId, u32, u32), SourceMapError> {
    if file_count >= FileId::DUMMY.as_u32() as usize {
        return Err(SourceMapError::FileIdExhausted { file_count });
    }
    let file_id = u32::try_from(file_count)
        .map(FileId::new)
        .map_err(|_| SourceMapError::FileIdExhausted { file_count })?;
    let end = checked_source_end(next_base, source_len)?;
    let following_base = end
        .checked_add(1)
        .ok_or(SourceMapError::PositionSpaceExhausted {
            base: next_base,
            bytes: source_len,
        })?;
    Ok((file_id, next_base, following_base))
}

impl fmt::Debug for SourceMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceMap")
            .field("file_count", &self.files.len())
            .field("next_base", &self.next_base)
            .finish()
    }
}

impl SourceProvider for SourceMap {
    fn read_source(&self, path: &str) -> Option<String> {
        self.files
            .iter()
            .find(|f| f.name() == path)
            .map(|f| f.source().to_string())
    }
}

/// A location in a source file, combining file ID and span.
///
/// Note: With global position space, Span alone is usually sufficient.
/// This type pairs a file id with a span.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    /// The file containing this location.
    pub file: FileId,
    /// The span within the file.
    pub span: Span,
}

impl SourceLocation {
    /// Creates a new source location.
    #[inline]
    pub const fn new(file: FileId, span: Span) -> Self {
        Self { file, span }
    }

    /// Creates a dummy source location.
    #[inline]
    pub const fn dummy() -> Self {
        Self {
            file: FileId::DUMMY,
            span: Span::dummy(),
        }
    }

    /// Returns true if this is a dummy location.
    #[inline]
    pub const fn is_dummy(&self) -> bool {
        self.file.is_dummy() || self.span.is_dummy()
    }

    /// Merges two locations, assuming they are in the same file.
    pub fn merge(self, other: SourceLocation) -> SourceLocation {
        if self.is_dummy() {
            return other;
        }
        if other.is_dummy() {
            return self;
        }
        assert_eq!(
            self.file, other.file,
            "cannot merge source locations from different files"
        );
        SourceLocation {
            file: self.file,
            span: self.span.merge(other.span),
        }
    }
}

impl fmt::Debug for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{:?}", self.file, self.span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_id() {
        let id = FileId::new(5);
        assert_eq!(id.as_u32(), 5);
        assert!(!id.is_dummy());

        assert!(FileId::DUMMY.is_dummy());
    }

    #[test]
    fn test_line_col() {
        let lc = LineCol::new(10, 5);
        assert_eq!(lc.line, 10);
        assert_eq!(lc.column, 5);
        assert_eq!(format!("{}", lc), "10:5");
    }

    #[test]
    fn test_source_map_global_positions() {
        let mut map = SourceMap::new();

        // Add first file: 12 bytes, base = 0
        let id1 = map.add_file("file1.vo", "hello\nworld\n");
        // Add second file: 8 bytes, base = 13 (12 + 1 gap)
        let id2 = map.add_file("file2.vo", "content2");

        let file1 = map.get_file(id1).unwrap();
        let file2 = map.get_file(id2).unwrap();

        assert_eq!(file1.base(), 0);
        assert_eq!(file1.end_pos(), 12);
        assert_eq!(file2.base(), 13);
        assert_eq!(file2.end_pos(), 21);
    }

    #[test]
    fn source_map_rejects_reserved_file_id_without_mutation() {
        let error = checked_source_allocation(FileId::DUMMY.as_u32() as usize, 0, 0)
            .expect_err("the dummy file ID must remain reserved");
        assert_eq!(
            error,
            SourceMapError::FileIdExhausted {
                file_count: FileId::DUMMY.as_u32() as usize,
            }
        );
    }

    #[test]
    fn source_map_rejects_position_overflow_without_mutation() {
        let mut map = SourceMap::new();
        map.next_base = u32::MAX;

        let error = map
            .try_add_file("overflow.vo", "")
            .expect_err("the inter-file gap must fit in the position space");

        assert_eq!(
            error,
            SourceMapError::PositionSpaceExhausted {
                base: u32::MAX,
                bytes: 0,
            }
        );
        assert_eq!(map.file_count(), 0);
        assert_eq!(map.next_base, u32::MAX);
    }

    #[test]
    fn source_file_rejects_invalid_position_metadata() {
        assert_eq!(
            SourceFile::try_new(FileId::DUMMY, "dummy.vo", "", 0).unwrap_err(),
            SourceMapError::DummyFileId
        );
        assert_eq!(
            SourceFile::try_new(FileId::new(0), "overflow.vo", "x", u32::MAX).unwrap_err(),
            SourceMapError::PositionSpaceExhausted {
                base: u32::MAX,
                bytes: 1,
            }
        );
    }

    #[test]
    fn source_map_rejects_source_at_line_number_limit() {
        let bytes = u32::MAX as usize;
        assert_eq!(
            checked_source_end(0, bytes).unwrap_err(),
            SourceMapError::SourceTooLarge { bytes }
        );
    }

    #[test]
    fn test_lookup_file() {
        let mut map = SourceMap::new();

        let id1 = map.add_file("file1.vo", "abcde"); // base=0, end=5
        let id2 = map.add_file("file2.vo", "fghij"); // base=6, end=11

        // Positions 0-4 are bytes in file1; position 5 is its EOF boundary.
        assert_eq!(map.lookup_file(BytePos::new(0)).unwrap().id(), id1);
        assert_eq!(map.lookup_file(BytePos::new(4)).unwrap().id(), id1);
        assert_eq!(map.lookup_file(BytePos::new(5)).unwrap().id(), id1);

        // Positions 6-10 are bytes in file2; position 11 is its EOF boundary.
        assert_eq!(map.lookup_file(BytePos::new(6)).unwrap().id(), id2);
        assert_eq!(map.lookup_file(BytePos::new(10)).unwrap().id(), id2);
        assert_eq!(map.lookup_file(BytePos::new(11)).unwrap().id(), id2);
    }

    #[test]
    fn lookup_supports_empty_files_and_rejects_cross_file_spans() {
        let mut map = SourceMap::new();
        let empty = map.add_file("empty.vo", "");
        let nonempty = map.add_file("next.vo", "x");

        assert_eq!(map.lookup_file(BytePos::new(0)).unwrap().id(), empty);
        assert_eq!(map.lookup_span(Span::from_u32(0, 0)).unwrap().id(), empty);
        assert_eq!(map.lookup_file(BytePos::new(1)).unwrap().id(), nonempty);
        assert!(map.lookup_span(Span::from_u32(0, 1)).is_none());
        assert!(map.lookup_span(Span::from_u32(2, 1)).is_none());
        assert!(map.lookup_span(Span::dummy()).is_none());
        assert_eq!(map.format_span(Span::dummy()), "?:unknown");
        assert_eq!(map.format_span(Span::from_u32(0, 1)), "?:unknown");
    }

    #[test]
    fn test_global_line_col() {
        let mut map = SourceMap::new();

        let _id1 = map.add_file("file1.vo", "abc\ndef"); // base=0
        let id2 = map.add_file("file2.vo", "xyz\n123"); // base=8

        let file2 = map.get_file(id2).unwrap();

        // In file2, position 8 is 'x' (line 1, col 1)
        assert_eq!(file2.line_col(BytePos::new(8)), LineCol::new(1, 1));
        // Position 12 is '1' (line 2, col 1)
        assert_eq!(file2.line_col(BytePos::new(12)), LineCol::new(2, 1));
    }

    #[test]
    fn test_span_text_global() {
        let mut map = SourceMap::new();

        let _id1 = map.add_file("file1.vo", "hello"); // base=0
        let _id2 = map.add_file("file2.vo", "world"); // base=6

        // Get text from file1
        assert_eq!(map.span_text(Span::from_u32(0, 5)), Some("hello"));

        // Get text from file2
        assert_eq!(map.span_text(Span::from_u32(6, 11)), Some("world"));
    }

    #[test]
    fn test_format_pos() {
        let mut map = SourceMap::new();

        map.add_file("test.vo", "line1\nline2\nline3");

        assert_eq!(map.format_pos(BytePos::new(0)), "test.vo:1:1");
        assert_eq!(map.format_pos(BytePos::new(6)), "test.vo:2:1");
        assert_eq!(map.format_pos(BytePos::new(12)), "test.vo:3:1");
    }

    #[test]
    fn test_source_file_basic() {
        let file = SourceFile::new(FileId::new(0), "test.vo", "hello\nworld\n", 0);

        assert_eq!(file.name(), "test.vo");
        assert_eq!(file.source(), "hello\nworld\n");
        assert_eq!(file.len(), 12);
        assert!(!file.is_empty());
        assert_eq!(file.base(), 0);
        assert_eq!(file.end_pos(), 12);
    }

    #[test]
    fn test_source_file_lines() {
        let file = SourceFile::new(FileId::new(0), "test.vo", "line1\nline2\nline3", 0);

        assert_eq!(file.line_count(), 3);
        assert_eq!(file.line_start(0), Some(0));
        assert_eq!(file.line_start(1), Some(6));
        assert_eq!(file.line_start(2), Some(12));
        assert_eq!(file.line_start(3), None);

        assert_eq!(file.line_content(0), Some("line1"));
        assert_eq!(file.line_content(1), Some("line2"));
        assert_eq!(file.line_content(2), Some("line3"));
        assert_eq!(file.line_content(3), None);
    }

    #[test]
    fn test_source_file_line_col() {
        let file = SourceFile::new(FileId::new(0), "test.vo", "abc\ndefgh\nij", 100);

        // First line (with base offset 100)
        assert_eq!(file.line_col(BytePos(100)), LineCol::new(1, 1));
        assert_eq!(file.line_col(BytePos(102)), LineCol::new(1, 3));

        // Second line
        assert_eq!(file.line_col(BytePos(104)), LineCol::new(2, 1));
        assert_eq!(file.line_col(BytePos(107)), LineCol::new(2, 4));

        // Third line
        assert_eq!(file.line_col(BytePos(110)), LineCol::new(3, 1));
        assert_eq!(file.line_col(BytePos(111)), LineCol::new(3, 2));
    }

    #[test]
    fn test_source_file_span_text() {
        let file = SourceFile::new(FileId::new(0), "test.vo", "hello world", 50);

        assert_eq!(file.span_text(Span::from_u32(50, 55)), "hello");
        assert_eq!(file.span_text(Span::from_u32(56, 61)), "world");
        assert_eq!(file.span_text(Span::from_u32(50, 61)), "hello world");
        assert_eq!(file.try_span_text(Span::from_u32(49, 55)), None);
        assert_eq!(file.try_span_text(Span::from_u32(61, 62)), None);
        assert_eq!(file.try_span_text(Span::from_u32(55, 54)), None);
    }

    #[test]
    fn source_file_checked_position_conversions_and_utf8_spans() {
        let file = SourceFile::new(FileId::new(0), "unicode.vo", "aé", 100);

        assert_eq!(file.try_local_offset(BytePos::new(99)), None);
        assert_eq!(file.try_local_offset(BytePos::new(100)), Some(0));
        assert_eq!(file.try_local_offset(BytePos::new(103)), Some(3));
        assert_eq!(file.try_local_offset(BytePos::new(104)), None);
        assert_eq!(file.try_global_pos(0), Some(BytePos::new(100)));
        assert_eq!(file.try_global_pos(3), Some(BytePos::new(103)));
        assert_eq!(file.try_global_pos(4), None);

        assert_eq!(file.try_span_text(Span::from_u32(101, 103)), Some("é"));
        assert_eq!(file.try_span_text(Span::from_u32(101, 102)), None);
        assert_eq!(
            file.try_span_line_col(Span::from_u32(100, 103)),
            Some((LineCol::new(1, 1), LineCol::new(1, 4)))
        );
        assert_eq!(file.try_span_line_col(Span::dummy()), None);
    }

    #[test]
    fn test_empty_file() {
        let file = SourceFile::new(FileId::new(0), "empty.vo", "", 0);

        assert!(file.is_empty());
        assert_eq!(file.line_count(), 1);
        assert_eq!(file.line_content(0), Some(""));
    }

    #[test]
    fn test_single_line_no_newline() {
        let file = SourceFile::new(FileId::new(0), "test.vo", "single line", 0);

        assert_eq!(file.line_count(), 1);
        assert_eq!(file.line_content(0), Some("single line"));
    }

    #[test]
    fn test_trailing_newline() {
        let file = SourceFile::new(FileId::new(0), "test.vo", "line1\nline2\n", 0);

        assert_eq!(file.line_count(), 3);
        assert_eq!(file.line_content(0), Some("line1"));
        assert_eq!(file.line_content(1), Some("line2"));
        assert_eq!(file.line_content(2), Some(""));
    }

    #[test]
    fn test_source_location() {
        let loc = SourceLocation::new(FileId::new(0), Span::from_u32(10, 20));
        assert!(!loc.is_dummy());

        let dummy = SourceLocation::dummy();
        assert!(dummy.is_dummy());
    }

    #[test]
    fn test_source_location_merge() {
        let loc1 = SourceLocation::new(FileId::new(0), Span::from_u32(10, 20));
        let loc2 = SourceLocation::new(FileId::new(0), Span::from_u32(15, 30));
        let merged = loc1.merge(loc2);

        assert_eq!(merged.span.start.0, 10);
        assert_eq!(merged.span.end.0, 30);
        assert_eq!(SourceLocation::dummy().merge(loc1), loc1);
        assert_eq!(loc1.merge(SourceLocation::dummy()), loc1);

        let partial_dummy = SourceLocation::new(FileId::new(0), Span::dummy());
        assert!(partial_dummy.is_dummy());
        assert_eq!(partial_dummy.merge(loc1), loc1);
    }

    #[test]
    #[should_panic(expected = "cannot merge source locations from different files")]
    fn source_location_rejects_cross_file_merge() {
        let first = SourceLocation::new(FileId::new(0), Span::from_u32(0, 1));
        let second = SourceLocation::new(FileId::new(1), Span::from_u32(2, 3));
        let _ = first.merge(second);
    }
}
