//! Base64 encoding/decoding (pure logic).

use base64::{Engine as _, engine::general_purpose};

/// Encode bytes to base64 string (standard encoding).
pub fn encode_std(src: &[u8]) -> String {
    general_purpose::STANDARD.encode(src)
}

/// Decode base64 string to bytes (standard encoding).
pub fn decode_std(src: &str) -> Result<Vec<u8>, String> {
    general_purpose::STANDARD.decode(src).map_err(|e| e.to_string())
}

/// Encode bytes to base64 string (URL-safe encoding).
pub fn encode_url(src: &[u8]) -> String {
    general_purpose::URL_SAFE.encode(src)
}

/// Decode base64 string to bytes (URL-safe encoding).
pub fn decode_url(src: &str) -> Result<Vec<u8>, String> {
    general_purpose::URL_SAFE.decode(src).map_err(|e| e.to_string())
}

/// Encode bytes to base64 string (standard, no padding).
pub fn encode_std_no_pad(src: &[u8]) -> String {
    general_purpose::STANDARD_NO_PAD.encode(src)
}

/// Decode base64 string to bytes (standard, no padding).
pub fn decode_std_no_pad(src: &str) -> Result<Vec<u8>, String> {
    general_purpose::STANDARD_NO_PAD.decode(src).map_err(|e| e.to_string())
}

/// Encode bytes to base64 string (URL-safe, no padding).
pub fn encode_url_no_pad(src: &[u8]) -> String {
    general_purpose::URL_SAFE_NO_PAD.encode(src)
}

/// Decode base64 string to bytes (URL-safe, no padding).
pub fn decode_url_no_pad(src: &str) -> Result<Vec<u8>, String> {
    general_purpose::URL_SAFE_NO_PAD.decode(src).map_err(|e| e.to_string())
}

/// Return the length of encoding of n bytes.
pub fn encoded_len(n: usize) -> usize {
    (n + 2) / 3 * 4
}

/// Return the length of decoding of n bytes (may be up to 2 bytes larger).
pub fn decoded_len(n: usize) -> usize {
    n / 4 * 3
}
