//! UTF-8 decoding utilities.

/// Decode a single UTF-8 rune from bytes.
/// Returns (rune, width). For invalid UTF-8, returns (0xFFFD, 1).
pub fn decode_rune(bytes: &[u8]) -> (i32, usize) {
    if bytes.is_empty() {
        return (0xFFFD, 0);
    }
    
    let b0 = bytes[0];
    
    // 1-byte (ASCII): 0xxxxxxx
    if b0 < 0x80 {
        return (b0 as i32, 1);
    }
    
    // 2-byte: 110xxxxx 10xxxxxx
    if b0 & 0xE0 == 0xC0 && bytes.len() >= 2 {
        let b1 = bytes[1];
        if b1 & 0xC0 == 0x80 {
            let r = ((b0 as i32 & 0x1F) << 6) | (b1 as i32 & 0x3F);
            if r >= 0x80 {
                return (r, 2);
            }
        }
    }
    
    // 3-byte: 1110xxxx 10xxxxxx 10xxxxxx
    if b0 & 0xF0 == 0xE0 && bytes.len() >= 3 {
        let b1 = bytes[1];
        let b2 = bytes[2];
        if b1 & 0xC0 == 0x80 && b2 & 0xC0 == 0x80 {
            let r = ((b0 as i32 & 0x0F) << 12) | ((b1 as i32 & 0x3F) << 6) | (b2 as i32 & 0x3F);
            if r >= 0x800 && !(0xD800..=0xDFFF).contains(&r) {
                return (r, 3);
            }
        }
    }
    
    // 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    if b0 & 0xF8 == 0xF0 && bytes.len() >= 4 {
        let b1 = bytes[1];
        let b2 = bytes[2];
        let b3 = bytes[3];
        if b1 & 0xC0 == 0x80 && b2 & 0xC0 == 0x80 && b3 & 0xC0 == 0x80 {
            let r = ((b0 as i32 & 0x07) << 18) | ((b1 as i32 & 0x3F) << 12) 
                  | ((b2 as i32 & 0x3F) << 6) | (b3 as i32 & 0x3F);
            if r >= 0x10000 && r <= 0x10FFFF {
                return (r, 4);
            }
        }
    }
    
    // Invalid UTF-8: return replacement character and advance 1 byte
    (0xFFFD, 1)
}

/// Unicode replacement character returned for invalid UTF-8.
pub const RUNE_ERROR: i32 = 0xFFFD;
