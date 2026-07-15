//! Source location tracking types.
//!
//! This module provides types for tracking locations in source code,
//! enabling accurate error reporting and source mapping.

use std::fmt;
use std::num::TryFromIntError;
use std::ops::Range;

/// An absolute byte position in a source file.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BytePos(pub u32);

impl BytePos {
    /// Creates a new byte position.
    #[inline]
    pub const fn new(pos: u32) -> Self {
        Self(pos)
    }

    /// Returns the byte position as a usize.
    #[inline]
    pub const fn to_usize(self) -> usize {
        self.0 as usize
    }

    /// Returns the byte position as a u32.
    #[inline]
    pub const fn to_u32(self) -> u32 {
        self.0
    }
}

impl From<u32> for BytePos {
    #[inline]
    fn from(pos: u32) -> Self {
        Self(pos)
    }
}

impl TryFrom<usize> for BytePos {
    type Error = TryFromIntError;

    #[inline]
    fn try_from(pos: usize) -> Result<Self, Self::Error> {
        u32::try_from(pos).map(Self)
    }
}

impl fmt::Debug for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BytePos({})", self.0)
    }
}

impl fmt::Display for BytePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A span representing a range of bytes in a source file.
///
/// Spans are half-open intervals: `[start, end)`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// The start byte position (inclusive).
    pub start: BytePos,
    /// The end byte position (exclusive).
    pub end: BytePos,
}

impl Span {
    /// Creates a new span from start and end positions.
    #[inline]
    pub const fn new(start: BytePos, end: BytePos) -> Self {
        Self { start, end }
    }

    /// Creates a new span from raw u32 positions.
    #[inline]
    pub const fn from_u32(start: u32, end: u32) -> Self {
        Self {
            start: BytePos(start),
            end: BytePos(end),
        }
    }

    /// Creates a dummy span (used for generated code or when location is unknown).
    ///
    /// The reversed range cannot collide with a valid source span, including an
    /// empty span at the first byte of the first source file.
    #[inline]
    pub const fn dummy() -> Self {
        Self {
            start: BytePos(u32::MAX),
            end: BytePos(0),
        }
    }

    /// Returns true if this is a dummy span.
    #[inline]
    pub const fn is_dummy(&self) -> bool {
        self.start.0 == u32::MAX && self.end.0 == 0
    }

    /// Returns true when the span is a valid half-open byte range.
    ///
    /// This checks only the ordering of the two global positions. Whether the
    /// range belongs to a particular source file is a [`SourceMap`](crate::SourceMap)
    /// concern.
    #[inline]
    pub const fn is_well_formed(&self) -> bool {
        self.start.0 <= self.end.0
    }

    /// Returns the length of the span in bytes.
    #[inline]
    pub const fn len(&self) -> u32 {
        self.end.0.saturating_sub(self.start.0)
    }

    /// Returns true if the span is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.start.0 >= self.end.0
    }

    /// Returns true if this span contains the given byte position.
    #[inline]
    pub const fn contains(&self, pos: BytePos) -> bool {
        self.is_well_formed() && self.start.0 <= pos.0 && pos.0 < self.end.0
    }

    /// Returns true if this span contains the given span.
    #[inline]
    pub const fn contains_span(&self, other: Span) -> bool {
        self.is_well_formed()
            && other.is_well_formed()
            && self.start.0 <= other.start.0
            && other.end.0 <= self.end.0
    }

    /// Returns the union of two spans (smallest span containing both).
    #[inline]
    pub fn merge(self, other: Span) -> Span {
        assert!(
            self.is_dummy() || self.is_well_formed(),
            "cannot merge malformed source spans"
        );
        assert!(
            other.is_dummy() || other.is_well_formed(),
            "cannot merge malformed source spans"
        );
        if self.is_dummy() {
            return other;
        }
        if other.is_dummy() {
            return self;
        }
        Span {
            start: BytePos(self.start.0.min(other.start.0)),
            end: BytePos(self.end.0.max(other.end.0)),
        }
    }

    /// Creates a span that starts at this span's start and ends at the other span's end.
    #[inline]
    pub const fn to(self, other: Span) -> Span {
        assert!(
            self.is_dummy() || self.is_well_formed(),
            "cannot join malformed source spans"
        );
        assert!(
            other.is_dummy() || other.is_well_formed(),
            "cannot join malformed source spans"
        );
        if self.is_dummy() {
            return other;
        }
        if other.is_dummy() {
            return self;
        }
        assert!(
            self.start.0 <= other.end.0,
            "cannot join source spans in reverse order"
        );
        Span {
            start: self.start,
            end: other.end,
        }
    }

    /// Creates a span that starts at the other span's start and ends at this span's end.
    #[inline]
    pub const fn from(self, other: Span) -> Span {
        assert!(
            self.is_dummy() || self.is_well_formed(),
            "cannot join malformed source spans"
        );
        assert!(
            other.is_dummy() || other.is_well_formed(),
            "cannot join malformed source spans"
        );
        if self.is_dummy() {
            return other;
        }
        if other.is_dummy() {
            return self;
        }
        assert!(
            other.start.0 <= self.end.0,
            "cannot join source spans in reverse order"
        );
        Span {
            start: other.start,
            end: self.end,
        }
    }

    /// Shrinks the span by the given amount from both ends.
    #[inline]
    pub fn shrink(self, amount: u32) -> Span {
        if self.is_dummy() {
            return self;
        }

        assert!(
            self.is_well_formed(),
            "cannot shrink a malformed source span"
        );
        let start = self.start.0.saturating_add(amount);
        let end = self.end.0.saturating_sub(amount);
        if start <= end {
            return Span::from_u32(start, end);
        }

        // Once the two sides overlap, collapse at the range's midpoint rather
        // than manufacturing a second sentinel-like reversed span.
        let midpoint = self.start.0 + self.len() / 2;
        Span::from_u32(midpoint, midpoint)
    }

    /// Converts to a Range<usize> for indexing.
    #[inline]
    pub fn to_range(&self) -> Range<usize> {
        if self.is_dummy() {
            0..0
        } else {
            assert!(
                self.is_well_formed(),
                "cannot convert a malformed source span to a range"
            );
            self.start.to_usize()..self.end.to_usize()
        }
    }

    /// Converts a well-formed, located span to an indexing range.
    ///
    /// Unknown and malformed spans return `None`. Use [`Span::to_range`] when
    /// the caller has already established that a span is suitable for source
    /// indexing and wants malformed metadata to fail loudly.
    #[inline]
    pub fn try_to_range(&self) -> Option<Range<usize>> {
        (!self.is_dummy() && self.is_well_formed())
            .then(|| self.start.to_usize()..self.end.to_usize())
    }
}

impl Default for Span {
    #[inline]
    fn default() -> Self {
        Self::dummy()
    }
}

impl From<Range<u32>> for Span {
    #[inline]
    fn from(range: Range<u32>) -> Self {
        Self::from_u32(range.start, range.end)
    }
}

impl TryFrom<Range<usize>> for Span {
    type Error = TryFromIntError;

    #[inline]
    fn try_from(range: Range<usize>) -> Result<Self, Self::Error> {
        Ok(Self::from_u32(
            u32::try_from(range.start)?,
            u32::try_from(range.end)?,
        ))
    }
}

impl From<Span> for Range<usize> {
    #[inline]
    fn from(span: Span) -> Self {
        span.to_range()
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_dummy() {
            f.write_str("<unknown>")
        } else {
            write!(f, "{}..{}", self.start.0, self.end.0)
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_dummy() {
            f.write_str("<unknown>")
        } else {
            write!(f, "{}..{}", self.start.0, self.end.0)
        }
    }
}

/// A value with an associated source span.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    /// The wrapped value.
    pub node: T,
    /// The source span.
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Creates a new spanned value.
    #[inline]
    pub const fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    /// Maps the inner value while preserving the span.
    #[inline]
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }

    /// Returns a reference to the inner value.
    #[inline]
    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            node: &self.node,
            span: self.span,
        }
    }

    /// Unwraps the spanned value, returning the inner value.
    #[inline]
    pub fn into_inner(self) -> T {
        self.node
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} @ {:?}", self.node, self.span)
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.node)
    }
}

impl<T: Default> Default for Spanned<T> {
    fn default() -> Self {
        Self {
            node: T::default(),
            span: Span::dummy(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_byte_pos() {
        let pos = BytePos::new(42);
        assert_eq!(pos.to_usize(), 42);
        assert_eq!(pos.to_u32(), 42);

        let pos_from_u32: BytePos = 100u32.into();
        assert_eq!(pos_from_u32.0, 100);

        let pos_from_usize = BytePos::try_from(200usize).unwrap();
        assert_eq!(pos_from_usize.0, 200);

        #[cfg(target_pointer_width = "64")]
        assert!(BytePos::try_from(u32::MAX as usize + 1).is_err());
    }

    #[test]
    fn test_span_creation() {
        let span = Span::new(BytePos(10), BytePos(20));
        assert_eq!(span.start.0, 10);
        assert_eq!(span.end.0, 20);
        assert_eq!(span.len(), 10);
        assert!(!span.is_empty());

        let span2 = Span::from_u32(5, 15);
        assert_eq!(span2.start.0, 5);
        assert_eq!(span2.end.0, 15);
    }

    #[test]
    fn test_span_dummy() {
        let dummy = Span::dummy();
        assert!(dummy.is_dummy());
        assert_eq!(Span::default(), dummy);
        assert!(dummy.is_empty());
        assert_eq!(dummy.len(), 0);
        assert_eq!(dummy.to_range(), 0..0);
        assert_eq!(dummy.try_to_range(), None);
        assert_eq!(dummy.to_string(), "<unknown>");
        assert!(!Span::from_u32(0, 0).is_dummy());
        assert!(Span::from_u32(0, 0).is_well_formed());
        assert!(!dummy.is_well_formed());
    }

    #[test]
    fn test_span_contains() {
        let span = Span::from_u32(10, 20);

        assert!(span.contains(BytePos(10)));
        assert!(span.contains(BytePos(15)));
        assert!(span.contains(BytePos(19)));
        assert!(!span.contains(BytePos(9)));
        assert!(!span.contains(BytePos(20)));
        assert!(!span.contains(BytePos(25)));
    }

    #[test]
    fn test_span_contains_span() {
        let outer = Span::from_u32(10, 30);
        let inner = Span::from_u32(15, 25);
        let overlapping = Span::from_u32(5, 20);

        assert!(outer.contains_span(inner));
        assert!(outer.contains_span(outer));
        assert!(!outer.contains_span(overlapping));
        assert!(!inner.contains_span(outer));
        assert!(!outer.contains_span(Span::dummy()));
        assert!(!Span::dummy().contains_span(outer));
        assert!(!outer.contains_span(Span::from_u32(20, 10)));
    }

    #[test]
    fn test_span_merge() {
        let span1 = Span::from_u32(10, 20);
        let span2 = Span::from_u32(15, 30);
        let merged = span1.merge(span2);

        assert_eq!(merged.start.0, 10);
        assert_eq!(merged.end.0, 30);
        assert_eq!(Span::dummy().merge(span1), span1);
        assert_eq!(span1.merge(Span::dummy()), span1);
    }

    #[test]
    fn test_span_to() {
        let span1 = Span::from_u32(10, 20);
        let span2 = Span::from_u32(25, 35);
        let combined = span1.to(span2);

        assert_eq!(combined.start.0, 10);
        assert_eq!(combined.end.0, 35);
        assert_eq!(Span::dummy().to(span2), span2);
        assert_eq!(span1.to(Span::dummy()), span1);
        assert_eq!(Span::dummy().from(span2), span2);
        assert_eq!(span1.from(Span::dummy()), span1);
    }

    #[test]
    fn test_span_shrink() {
        let span = Span::from_u32(10, 30);
        let shrunk = span.shrink(5);

        assert_eq!(shrunk.start.0, 15);
        assert_eq!(shrunk.end.0, 25);
        assert_eq!(span.shrink(20), Span::from_u32(20, 20));
        assert_eq!(Span::dummy().shrink(5), Span::dummy());
    }

    #[test]
    fn test_span_to_range() {
        let span = Span::from_u32(10, 20);
        let range = span.to_range();

        assert_eq!(range, 10..20);
        assert_eq!(span.try_to_range(), Some(10..20));
    }

    #[test]
    #[should_panic(expected = "cannot convert a malformed source span to a range")]
    fn malformed_span_cannot_be_converted_to_range() {
        let _ = Span::from_u32(20, 10).to_range();
    }

    #[test]
    #[should_panic(expected = "cannot merge malformed source spans")]
    fn malformed_span_cannot_be_merged() {
        let _ = Span::from_u32(20, 10).merge(Span::from_u32(30, 40));
    }

    #[test]
    #[should_panic(expected = "cannot merge malformed source spans")]
    fn dummy_span_does_not_mask_a_malformed_merge_operand() {
        let _ = Span::dummy().merge(Span::from_u32(20, 10));
    }

    #[test]
    fn test_span_from_range() {
        let span: Span = (10u32..20u32).into();
        assert_eq!(span.start.0, 10);
        assert_eq!(span.end.0, 20);

        let span2 = Span::try_from(5usize..15usize).unwrap();
        assert_eq!(span2.start.0, 5);
        assert_eq!(span2.end.0, 15);

        #[cfg(target_pointer_width = "64")]
        assert!(Span::try_from(0usize..u32::MAX as usize + 1).is_err());
    }

    #[test]
    fn test_spanned() {
        let spanned = Spanned::new(42, Span::from_u32(0, 2));
        assert_eq!(spanned.node, 42);
        assert_eq!(spanned.span.start.0, 0);
        assert_eq!(spanned.span.end.0, 2);
    }

    #[test]
    fn test_spanned_map() {
        let spanned = Spanned::new(10, Span::from_u32(0, 5));
        let mapped = spanned.map(|x| x * 2);

        assert_eq!(mapped.node, 20);
        assert_eq!(mapped.span, Span::from_u32(0, 5));
    }

    #[test]
    fn test_spanned_into_inner() {
        let spanned = Spanned::new("hello", Span::from_u32(0, 5));
        assert_eq!(spanned.into_inner(), "hello");
    }
}
