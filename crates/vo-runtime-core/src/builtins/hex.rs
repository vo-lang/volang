//! Hex encoding/decoding (pure logic).

const HEX_CHARS: &[u8; 16] = b"0123456789abcdef";

/// Encode bytes to hex string.
pub fn encode(src: &[u8]) -> String {
    let mut dst = String::with_capacity(src.len() * 2);
    for &b in src {
        dst.push(HEX_CHARS[(b >> 4) as usize] as char);
        dst.push(HEX_CHARS[(b & 0x0f) as usize] as char);
    }
    dst
}

/// Decode hex string to bytes.
pub fn decode(src: &str) -> Result<Vec<u8>, String> {
    let src = src.as_bytes();
    if src.len() % 2 != 0 {
        return Err("odd length hex string".to_string());
    }
    
    let mut dst = Vec::with_capacity(src.len() / 2);
    for chunk in src.chunks(2) {
        let hi = from_hex_char(chunk[0])?;
        let lo = from_hex_char(chunk[1])?;
        dst.push((hi << 4) | lo);
    }
    Ok(dst)
}

fn from_hex_char(c: u8) -> Result<u8, String> {
    match c {
        b'0'..=b'9' => Ok(c - b'0'),
        b'a'..=b'f' => Ok(c - b'a' + 10),
        b'A'..=b'F' => Ok(c - b'A' + 10),
        _ => Err(format!("invalid hex char: {}", c as char)),
    }
}

/// Return the length of encoding of n bytes.
pub fn encoded_len(n: usize) -> usize {
    n * 2
}

/// Return the length of decoding of n bytes.
pub fn decoded_len(n: usize) -> usize {
    n / 2
}
