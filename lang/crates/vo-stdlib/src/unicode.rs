//! unicode package native function implementations.
//!
//! All Unicode character classification and case conversion requires
//! Unicode tables, so they must be native.

use vo_ffi_macro::vostd_extern;

// ==================== Character classification ====================

#[vostd_extern("unicode", "IsLetter")]
fn is_letter(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(|c| c.is_alphabetic())
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsDigit")]
fn is_digit(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(|c| c.is_ascii_digit()) // Go's IsDigit is decimal digits only
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsSpace")]
fn is_space(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(|c| c.is_whitespace())
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsUpper")]
fn is_upper(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(|c| c.is_uppercase())
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsLower")]
fn is_lower(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(|c| c.is_lowercase())
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsControl")]
fn is_control(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(|c| c.is_control())
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsPrint")]
fn is_print(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(|c| !c.is_control() && c != '\u{FFFD}')
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsPunct")]
fn is_punct(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(|c| c.is_ascii_punctuation() || unicode_is_punct(c))
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsGraphic")]
fn is_graphic(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(|c| !c.is_control() && !c.is_whitespace() || c == ' ')
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsNumber")]
fn is_number(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(|c| c.is_numeric())
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsMark")]
fn is_mark(r: i32) -> bool {
    // Unicode Mark category (Mn, Mc, Me)
    char::from_u32(r as u32)
        .map(unicode_is_mark)
        .unwrap_or(false)
}

#[vostd_extern("unicode", "IsSymbol")]
fn is_symbol(r: i32) -> bool {
    char::from_u32(r as u32)
        .map(unicode_is_symbol)
        .unwrap_or(false)
}

// ==================== Case conversion ====================

#[vostd_extern("unicode", "ToLower")]
fn to_lower(r: i32) -> i32 {
    char::from_u32(r as u32)
        .and_then(|c| c.to_lowercase().next())
        .map(|c| c as i32)
        .unwrap_or(r)
}

#[vostd_extern("unicode", "ToUpper")]
fn to_upper(r: i32) -> i32 {
    char::from_u32(r as u32)
        .and_then(|c| c.to_uppercase().next())
        .map(|c| c as i32)
        .unwrap_or(r)
}

#[vostd_extern("unicode", "ToTitle")]
fn to_title(r: i32) -> i32 {
    // In most cases, title case is the same as uppercase
    char::from_u32(r as u32)
        .and_then(|c| c.to_uppercase().next())
        .map(|c| c as i32)
        .unwrap_or(r)
}

#[vostd_extern("unicode", "SimpleFold")]
fn simple_fold(r: i32) -> i32 {
    // SimpleFold returns the next character in the Unicode case fold orbit
    // For most characters, this cycles: lower -> upper -> lower
    let c = match char::from_u32(r as u32) {
        Some(c) => c,
        None => return r,
    };
    
    if c.is_uppercase() {
        c.to_lowercase().next().map(|c| c as i32).unwrap_or(r)
    } else if c.is_lowercase() {
        c.to_uppercase().next().map(|c| c as i32).unwrap_or(r)
    } else {
        r
    }
}

// ==================== Helper functions ====================

fn unicode_is_punct(c: char) -> bool {
    // Unicode punctuation categories: Pc, Pd, Pe, Pf, Pi, Po, Ps
    matches!(c,
        '\u{0021}'..='\u{002F}' |  // Basic Latin punctuation
        '\u{003A}'..='\u{0040}' |
        '\u{005B}'..='\u{0060}' |
        '\u{007B}'..='\u{007E}' |
        '\u{00A1}'..='\u{00BF}' |  // Latin-1 punctuation
        '\u{2010}'..='\u{2027}' |  // General punctuation
        '\u{2030}'..='\u{205E}' |
        '\u{3001}'..='\u{303F}'    // CJK punctuation
    )
}

fn unicode_is_mark(c: char) -> bool {
    // Unicode Mark categories: Mn (non-spacing), Mc (spacing combining), Me (enclosing)
    matches!(c as u32,
        0x0300..=0x036F |  // Combining Diacritical Marks
        0x0483..=0x0489 |  // Cyrillic combining marks
        0x0591..=0x05BD |  // Hebrew marks
        0x05BF |
        0x05C1..=0x05C2 |
        0x05C4..=0x05C5 |
        0x05C7 |
        0x0610..=0x061A |  // Arabic marks
        0x064B..=0x065F |
        0x0670 |
        0x06D6..=0x06DC |
        0x06DF..=0x06E4 |
        0x06E7..=0x06E8 |
        0x06EA..=0x06ED |
        0x0711 |
        0x0730..=0x074A |
        0x07A6..=0x07B0 |
        0x07EB..=0x07F3 |
        0x0816..=0x0819 |
        0x081B..=0x0823 |
        0x0825..=0x0827 |
        0x0829..=0x082D |
        0x0859..=0x085B |
        0x08D4..=0x08E1 |
        0x08E3..=0x0903 |
        0x093A..=0x093C |
        0x093E..=0x094F |
        0x0951..=0x0957 |
        0x0962..=0x0963 |
        0x0981..=0x0983 |
        0x09BC |
        0x09BE..=0x09C4 |
        0x09C7..=0x09C8 |
        0x09CB..=0x09CD |
        0x09D7 |
        0x09E2..=0x09E3 |
        0x0A01..=0x0A03 |
        0x0A3C |
        0x0A3E..=0x0A42 |
        0x0A47..=0x0A48 |
        0x0A4B..=0x0A4D |
        0x0A51 |
        0x0A70..=0x0A71 |
        0x0A75 |
        0x0A81..=0x0A83 |
        0x0ABC |
        0x0ABE..=0x0AC5 |
        0x0AC7..=0x0AC9 |
        0x0ACB..=0x0ACD |
        0x0AE2..=0x0AE3 |
        0x0B01..=0x0B03 |
        0x0B3C |
        0x0B3E..=0x0B44 |
        0x0B47..=0x0B48 |
        0x0B4B..=0x0B4D |
        0x0B56..=0x0B57 |
        0x0B62..=0x0B63 |
        0x0B82 |
        0x0BBE..=0x0BC2 |
        0x0BC6..=0x0BC8 |
        0x0BCA..=0x0BCD |
        0x0BD7 |
        0x0C00..=0x0C03 |
        0x0C3E..=0x0C44 |
        0x0C46..=0x0C48 |
        0x0C4A..=0x0C4D |
        0x0C55..=0x0C56 |
        0x0C62..=0x0C63 |
        0x0C81..=0x0C83 |
        0x0CBC |
        0x0CBE..=0x0CC4 |
        0x0CC6..=0x0CC8 |
        0x0CCA..=0x0CCD |
        0x0CD5..=0x0CD6 |
        0x0CE2..=0x0CE3 |
        0x0D01..=0x0D03 |
        0x0D3E..=0x0D44 |
        0x0D46..=0x0D48 |
        0x0D4A..=0x0D4D |
        0x0D57 |
        0x0D62..=0x0D63 |
        0x0D82..=0x0D83 |
        0x0DCA |
        0x0DCF..=0x0DD4 |
        0x0DD6 |
        0x0DD8..=0x0DDF |
        0x0DF2..=0x0DF3 |
        0xFE20..=0xFE2F    // Combining half marks
    )
}

fn unicode_is_symbol(c: char) -> bool {
    // Unicode Symbol categories: Sm (math), Sc (currency), Sk (modifier), So (other)
    matches!(c,
        '$' | '+' | '<' | '=' | '>' | '^' | '`' | '|' | '~' |
        '\u{00A2}'..='\u{00A5}' |  // Currency symbols
        '\u{00A6}'..='\u{00A9}' |  // Other symbols
        '\u{00AC}' |
        '\u{00AE}'..='\u{00B1}' |
        '\u{00B4}' |
        '\u{00B6}' |
        '\u{00B8}' |
        '\u{00D7}' |
        '\u{00F7}' |
        '\u{02C2}'..='\u{02C5}' |
        '\u{02D2}'..='\u{02DF}' |
        '\u{02E5}'..='\u{02EB}' |
        '\u{02ED}' |
        '\u{02EF}'..='\u{02FF}' |
        '\u{2000}'..='\u{206F}' |  // General punctuation (symbols)
        '\u{2100}'..='\u{214F}' |  // Letterlike symbols
        '\u{2190}'..='\u{21FF}' |  // Arrows
        '\u{2200}'..='\u{22FF}' |  // Mathematical operators
        '\u{2300}'..='\u{23FF}' |  // Miscellaneous technical
        '\u{2400}'..='\u{243F}' |  // Control pictures
        '\u{2440}'..='\u{245F}' |  // OCR
        '\u{2460}'..='\u{24FF}' |  // Enclosed alphanumerics
        '\u{2500}'..='\u{257F}' |  // Box drawing
        '\u{2580}'..='\u{259F}' |  // Block elements
        '\u{25A0}'..='\u{25FF}' |  // Geometric shapes
        '\u{2600}'..='\u{26FF}' |  // Miscellaneous symbols
        '\u{2700}'..='\u{27BF}' |  // Dingbats
        '\u{27C0}'..='\u{27EF}' |  // Misc mathematical symbols A
        '\u{27F0}'..='\u{27FF}' |  // Supplemental arrows A
        '\u{2800}'..='\u{28FF}' |  // Braille patterns
        '\u{2900}'..='\u{297F}' |  // Supplemental arrows B
        '\u{2980}'..='\u{29FF}' |  // Misc mathematical symbols B
        '\u{2A00}'..='\u{2AFF}' |  // Supplemental mathematical operators
        '\u{2B00}'..='\u{2BFF}'    // Misc symbols and arrows
    )
}

vo_runtime::stdlib_register!(unicode:
    IsLetter, IsDigit, IsSpace, IsUpper, IsLower, IsControl,
    IsPrint, IsPunct, IsGraphic, IsNumber, IsMark, IsSymbol,
    ToLower, ToUpper, ToTitle, SimpleFold,
);
