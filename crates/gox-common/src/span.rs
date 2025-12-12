//! Span representation for source code locations.
//!
//! A `Span` identifies a contiguous region of source code by byte offsets.

/// A span in the source code, represented as a byte offset range [start, end).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    /// Start byte offset (inclusive)
    pub start: usize,
    /// End byte offset (exclusive)
    pub end: usize,
}

impl Span {
    /// Create a new span from start to end byte offsets.
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end, "Span start must be <= end");
        Span { start, end }
    }

    /// Create a zero-length span at a position (useful for insertion points).
    pub fn point(pos: usize) -> Self {
        Span {
            start: pos,
            end: pos,
        }
    }

    /// Create a dummy/placeholder span for synthesized nodes.
    pub fn dummy() -> Self {
        Span { start: 0, end: 0 }
    }

    /// Check if this is a dummy span.
    pub fn is_dummy(&self) -> bool {
        self.start == 0 && self.end == 0
    }

    /// Length of the span in bytes.
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Check if span is empty.
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Merge two spans into one that covers both.
    /// The result spans from the minimum start to the maximum end.
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// Create a span that starts at self.start and ends at other.end.
    /// Useful for spanning "from X to Y" in AST construction.
    pub fn to(&self, other: &Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }

    /// Check if this span contains a byte offset.
    pub fn contains(&self, offset: usize) -> bool {
        self.start <= offset && offset < self.end
    }

    /// Check if this span fully contains another span.
    pub fn contains_span(&self, other: &Span) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    /// Convert to a Range for use with codespan-reporting.
    pub fn to_range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

impl From<Span> for std::ops::Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(range: std::ops::Range<usize>) -> Self {
        Span::new(range.start, range.end)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_merge() {
        let a = Span::new(10, 20);
        let b = Span::new(15, 30);
        let merged = a.merge(&b);
        assert_eq!(merged, Span::new(10, 30));
    }

    #[test]
    fn test_span_to() {
        let start = Span::new(5, 10);
        let end = Span::new(20, 25);
        assert_eq!(start.to(&end), Span::new(5, 25));
    }

    #[test]
    fn test_span_contains() {
        let span = Span::new(10, 20);
        assert!(span.contains(10));
        assert!(span.contains(15));
        assert!(!span.contains(20)); // exclusive end
        assert!(!span.contains(5));
    }
}
