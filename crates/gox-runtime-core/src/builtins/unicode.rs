//! Unicode operations (pure logic).

/// Check if char is a letter.
pub fn is_letter(c: char) -> bool {
    c.is_alphabetic()
}

/// Check if char is a digit.
pub fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

/// Check if char is whitespace.
pub fn is_space(c: char) -> bool {
    c.is_whitespace()
}

/// Check if char is uppercase.
pub fn is_upper(c: char) -> bool {
    c.is_uppercase()
}

/// Check if char is lowercase.
pub fn is_lower(c: char) -> bool {
    c.is_lowercase()
}

/// Check if char is alphanumeric.
pub fn is_alpha_numeric(c: char) -> bool {
    c.is_alphanumeric()
}

/// Check if char is a control character.
pub fn is_control(c: char) -> bool {
    c.is_control()
}

/// Check if char is a graphic character.
pub fn is_graphic(c: char) -> bool {
    !c.is_control() && !c.is_whitespace()
}

/// Check if char is printable.
pub fn is_print(c: char) -> bool {
    !c.is_control()
}

/// Check if char is punctuation.
pub fn is_punct(c: char) -> bool {
    c.is_ascii_punctuation()
}

/// Convert char to lowercase.
pub fn to_lower(c: char) -> char {
    c.to_lowercase().next().unwrap_or(c)
}

/// Convert char to uppercase.
pub fn to_upper(c: char) -> char {
    c.to_uppercase().next().unwrap_or(c)
}

/// Convert char to title case.
pub fn to_title(c: char) -> char {
    c.to_uppercase().next().unwrap_or(c)
}
