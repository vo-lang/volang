//! LSP uses UTF-16 positions and treats CR, LF and CRLF as line endings.
//! Keep the original compiler bytes; only the transport coordinates change.

use lsp_types::Position;

pub(super) struct Coordinates<'a> {
    text: &'a str,
    lines: Vec<(usize, usize)>,
}

impl<'a> Coordinates<'a> {
    pub fn new(text: &'a str) -> Self {
        let mut lines = Vec::new();
        let (mut start, mut at) = (0, 0);
        let bytes = text.as_bytes();
        while at < bytes.len() {
            if matches!(bytes[at], b'\r' | b'\n') {
                lines.push((start, at));
                if bytes[at] == b'\r' && bytes.get(at + 1) == Some(&b'\n') {
                    at += 1;
                }
                start = at + 1;
            }
            at += 1;
        }
        lines.push((start, text.len()));
        Self { text, lines }
    }

    pub fn offset(&self, position: Position) -> Option<u32> {
        let &(start, end) = self.lines.get(position.line as usize)?;
        let mut units = 0;
        for (offset, scalar) in self.text[start..end].char_indices() {
            if units == position.character {
                return u32::try_from(start + offset).ok();
            }
            units += scalar.len_utf16() as u32;
            if units > position.character {
                return None;
            }
        }
        (units == position.character)
            .then(|| u32::try_from(end).ok())
            .flatten()
    }

    pub fn position(&self, offset: u32) -> Option<Position> {
        let offset = offset as usize;
        if !self.text.is_char_boundary(offset) {
            return None;
        }
        let line = self
            .lines
            .partition_point(|&(start, _)| start <= offset)
            .checked_sub(1)?;
        let (start, end) = self.lines[line];
        let character = self.text[start..offset.min(end)].encode_utf16().count();
        Some(Position::new(
            u32::try_from(line).ok()?,
            u32::try_from(character).ok()?,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn positions_preserve_unicode_and_every_line_ending() {
        let text = "中🙂x\r\n\rbare\nlast\r";
        let positions = Coordinates::new(text);
        for (line, column, byte) in [
            (0, 0, 0),
            (0, 1, 3),
            (0, 3, 7),
            (0, 4, 8),
            (1, 0, 10),
            (2, 0, 11),
            (3, 0, 16),
            (4, 0, 21),
        ] {
            let position = Position::new(line, column);
            assert_eq!(positions.offset(position), Some(byte));
            assert_eq!(positions.position(byte), Some(position));
        }
        assert!(positions.offset(Position::new(0, 2)).is_none());
        assert!(positions.offset(Position::new(0, 5)).is_none());
        assert!(positions.offset(Position::new(5, 0)).is_none());
        assert!(positions.position(1).is_none());
        assert!(positions.position(22).is_none());
        assert_eq!(Coordinates::new("").offset(Position::new(0, 0)), Some(0));
    }
}
