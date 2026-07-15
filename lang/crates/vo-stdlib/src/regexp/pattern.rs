//! Go-compatible regexp pattern normalization and validation.
//!
//! Rust's `regex` crate is an excellent linear-time engine, but its surface
//! syntax intentionally differs from Go's `regexp` package. This module keeps
//! that difference at the boundary: it parses the Go-specific pieces, emits a
//! semantically equivalent internal pattern, validates Go's resource limits,
//! and hides internal capture names from the public API.

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec, vec::Vec};

use core::fmt::Write as _;
use core::ops::Deref;

use icu_properties::props::Script;
use icu_properties::{PropertyNamesLong, PropertyParser};
use regex::{Regex, RegexBuilder};
use regex_syntax::ast::{self, Ast};

const GO_MAX_NESTING: u32 = 1_000;
const GO_MAX_REPEAT_PRODUCT: u32 = 1_000;
const GO_FLAGS: [u8; 4] = [b'i', b'm', b's', b'U'];
const UNGREEDY_FLAG: usize = 3;

pub(super) struct CompiledRegex {
    regex: Regex,
    capture_names: Vec<Option<String>>,
    expression_is_empty: bool,
}

impl CompiledRegex {
    pub(super) fn capture_names_go(&self) -> &[Option<String>] {
        &self.capture_names
    }

    pub(super) fn expression_is_empty(&self) -> bool {
        self.expression_is_empty
    }
}

impl Deref for CompiledRegex {
    type Target = Regex;

    fn deref(&self) -> &Self::Target {
        &self.regex
    }
}

pub(super) fn compile(pattern: &[u8]) -> Option<CompiledRegex> {
    let expression = core::str::from_utf8(pattern).ok()?;
    let normalized = Normalizer::new(expression).normalize()?;

    let mut parser_builder = ast::parse::ParserBuilder::new();
    parser_builder.nest_limit(GO_MAX_NESTING);
    let ast = parser_builder.build().parse(&normalized.pattern).ok()?;
    ast::visit(&ast, GoAstValidator::new()).ok()?;

    let regex = RegexBuilder::new(&normalized.pattern)
        .nest_limit(GO_MAX_NESTING)
        .build()
        .ok()?;
    if regex.captures_len() != normalized.capture_names.len() {
        return None;
    }

    Some(CompiledRegex {
        regex,
        capture_names: normalized.capture_names,
        expression_is_empty: expression.is_empty(),
    })
}

struct NormalizedPattern {
    pattern: String,
    capture_names: Vec<Option<String>>,
}

struct Normalizer<'a> {
    source: &'a str,
    bytes: &'a [u8],
    pos: usize,
    pattern: String,
    capture_names: Vec<Option<String>>,
    flags: [bool; GO_FLAGS.len()],
    flag_stack: Vec<[bool; GO_FLAGS.len()]>,
    group_atom_starts: Vec<usize>,
    pending_flag_base: Option<[bool; GO_FLAGS.len()]>,
    transparent_separator: bool,
    transparent_requires_group: bool,
    last_atom_start: Option<usize>,
    previous_was_repetition: bool,
}

impl<'a> Normalizer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            pos: 0,
            pattern: String::with_capacity(source.len()),
            capture_names: vec![None],
            flags: [false; GO_FLAGS.len()],
            flag_stack: Vec::new(),
            group_atom_starts: Vec::new(),
            pending_flag_base: None,
            transparent_separator: false,
            transparent_requires_group: false,
            last_atom_start: None,
            previous_was_repetition: false,
        }
    }

    fn normalize(mut self) -> Option<NormalizedPattern> {
        while self.pos < self.bytes.len() {
            match self.bytes[self.pos] {
                b'\\' => self.normalize_escape()?,
                b'[' => self.normalize_class()?,
                b'(' => self.normalize_group_open()?,
                b')' => self.normalize_group_close(),
                b'*' | b'+' | b'?' => self.normalize_simple_repetition()?,
                b'{' => self.normalize_counted_repetition()?,
                _ => self.copy_source_char(),
            }
        }
        self.flush_pending_flags();
        Some(NormalizedPattern {
            pattern: self.pattern,
            capture_names: self.capture_names,
        })
    }

    fn copy_source_char(&mut self) {
        self.flush_pending_flags();
        let atom_start = self.pattern.len();
        let character = self.source[self.pos..]
            .chars()
            .next()
            .expect("position is inside a validated UTF-8 pattern");
        self.pattern.push(character);
        self.pos += character.len_utf8();
        self.transparent_separator = false;
        self.transparent_requires_group = false;
        if character == '|' {
            self.last_atom_start = None;
        } else {
            self.last_atom_start = Some(atom_start);
        }
        self.previous_was_repetition = false;
    }

    fn normalize_group_close(&mut self) {
        self.flush_pending_flags();
        self.pattern.push(')');
        self.pos += 1;
        if let Some(flags) = self.flag_stack.pop() {
            self.flags = flags;
        }
        if let Some(start) = self.group_atom_starts.pop() {
            self.last_atom_start = Some(start);
        }
        self.transparent_separator = false;
        self.transparent_requires_group = false;
        self.previous_was_repetition = false;
    }

    fn flush_pending_flags(&mut self) {
        let Some(base) = self.pending_flag_base.take() else {
            return;
        };
        self.push_flag_transition(base, self.flags);
    }

    fn push_flag_transition(
        &mut self,
        base: [bool; GO_FLAGS.len()],
        target: [bool; GO_FLAGS.len()],
    ) {
        let has_enabled = (0..GO_FLAGS.len()).any(|index| !base[index] && target[index]);
        let has_disabled = (0..GO_FLAGS.len()).any(|index| base[index] && !target[index]);
        if !has_enabled && !has_disabled {
            return;
        }

        self.pattern.push_str("(?");
        for (index, &flag) in GO_FLAGS.iter().enumerate() {
            if !base[index] && target[index] {
                self.pattern.push(char::from(flag));
            }
        }
        if has_disabled {
            self.pattern.push('-');
            for (index, &flag) in GO_FLAGS.iter().enumerate() {
                if base[index] && !target[index] {
                    self.pattern.push(char::from(flag));
                }
            }
        }
        self.pattern.push(')');
    }

    fn normalize_group_open(&mut self) -> Option<()> {
        if self.bytes.get(self.pos + 1) != Some(&b'?') {
            self.flush_pending_flags();
            let group_start = self.pattern.len();
            self.pattern.push('(');
            self.capture_names.push(None);
            self.flag_stack.push(self.flags);
            self.group_atom_starts.push(group_start);
            self.last_atom_start = None;
            self.transparent_separator = false;
            self.transparent_requires_group = false;
            self.previous_was_repetition = false;
            self.pos += 1;
            return Some(());
        }

        let name_start = if self.bytes.get(self.pos + 2) == Some(&b'P')
            && self.bytes.get(self.pos + 3) == Some(&b'<')
        {
            Some(self.pos + 4)
        } else if self.bytes.get(self.pos + 2) == Some(&b'<') {
            Some(self.pos + 3)
        } else {
            None
        };
        if let Some(name_start) = name_start {
            let relative_end = self.bytes[name_start..]
                .iter()
                .position(|&byte| byte == b'>')?;
            let name_end = name_start + relative_end;
            let name = &self.source[name_start..name_end];
            if name.is_empty()
                || !name
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
            {
                return None;
            }

            self.flush_pending_flags();
            let group_start = self.pattern.len();
            let capture_index = self.capture_names.len();
            self.capture_names.push(Some(name.into()));
            self.flag_stack.push(self.flags);
            self.group_atom_starts.push(group_start);
            self.last_atom_start = None;
            self.transparent_separator = false;
            self.transparent_requires_group = false;
            self.previous_was_repetition = false;
            write!(self.pattern, "(?P<__vo_capture_{capture_index}>").ok()?;
            self.pos = name_end + 1;
            return Some(());
        }

        self.normalize_flag_group()
    }

    fn normalize_flag_group(&mut self) -> Option<()> {
        let mut enabled = [false; GO_FLAGS.len()];
        let mut disabled = [false; GO_FLAGS.len()];
        let mut clearing = false;
        let mut saw_cleared_flag = false;
        let mut cursor = self.pos + 2;

        loop {
            let byte = *self.bytes.get(cursor)?;
            if let Some(index) = GO_FLAGS.iter().position(|&flag| flag == byte) {
                if clearing {
                    disabled[index] = true;
                    saw_cleared_flag = true;
                } else {
                    enabled[index] = true;
                }
                cursor += 1;
                continue;
            }
            if byte == b'-' {
                if clearing {
                    return None;
                }
                clearing = true;
                cursor += 1;
                continue;
            }
            if byte != b':' && byte != b')' {
                // This rejects look-around, Rust-only flags (u/R/x), and all
                // other Perl extensions absent from Go's regexp syntax.
                return None;
            }
            if clearing && !saw_cleared_flag {
                return None;
            }

            for index in 0..GO_FLAGS.len() {
                if disabled[index] {
                    enabled[index] = false;
                }
            }
            let mut next_flags = self.flags;
            for index in 0..GO_FLAGS.len() {
                if enabled[index] {
                    next_flags[index] = true;
                }
                if disabled[index] {
                    next_flags[index] = false;
                }
            }
            let has_enabled = enabled.iter().any(|&value| value);
            let has_disabled = disabled.iter().any(|&value| value);
            if byte == b':' {
                self.flush_pending_flags();
                self.flag_stack.push(self.flags);
                self.group_atom_starts.push(self.pattern.len());
                self.last_atom_start = None;
                self.transparent_separator = false;
                self.transparent_requires_group = false;
                if !has_enabled && !has_disabled {
                    self.pattern.push_str("(?:");
                } else {
                    self.pattern.push_str("(?");
                    for (index, &flag) in GO_FLAGS.iter().enumerate() {
                        if enabled[index] {
                            self.pattern.push(char::from(flag));
                        }
                    }
                    if has_disabled {
                        self.pattern.push('-');
                        for (index, &flag) in GO_FLAGS.iter().enumerate() {
                            if disabled[index] {
                                self.pattern.push(char::from(flag));
                            }
                        }
                    }
                    self.pattern.push(':');
                }
            } else {
                self.pending_flag_base.get_or_insert(self.flags);
                if !self.transparent_separator {
                    self.transparent_requires_group = self.previous_was_repetition;
                }
                self.transparent_separator = true;
            }
            self.previous_was_repetition = false;
            self.flags = next_flags;
            self.pos = cursor + 1;
            return Some(());
        }
    }

    fn normalize_escape(&mut self) -> Option<()> {
        let escaped = *self.bytes.get(self.pos + 1)?;
        if escaped == b'Q' {
            return self.normalize_quoted_literal();
        }
        self.flush_pending_flags();
        let atom_start = self.pattern.len();
        match escaped {
            b'A' | b'z' => {
                self.pattern.push('\\');
                self.pattern.push(char::from(escaped));
                self.pos += 2;
                Some(())
            }
            b'b' | b'B' => {
                // Go's word boundary is defined in terms of ASCII \w even
                // while the rest of the expression remains Unicode-aware.
                self.pattern.push_str("(?-u:\\");
                self.pattern.push(char::from(escaped));
                self.pattern.push(')');
                self.pos += 2;
                Some(())
            }
            b'd' | b'D' | b's' | b'S' | b'w' | b'W' => {
                push_perl_class(&mut self.pattern, escaped);
                self.pos += 2;
                Some(())
            }
            b'p' | b'P' => self.normalize_unicode_class(),
            b'C' | b'u' | b'U' => None,
            _ => {
                let (rune, end) = parse_literal_escape(self.source, self.pos)?;
                push_pattern_rune(&mut self.pattern, rune)?;
                self.pos = end;
                Some(())
            }
        }?;
        self.last_atom_start = Some(atom_start);
        self.transparent_separator = false;
        self.transparent_requires_group = false;
        self.previous_was_repetition = false;
        Some(())
    }

    fn normalize_simple_repetition(&mut self) -> Option<()> {
        if self.previous_was_repetition {
            return None;
        }
        let start = self.pos;
        let end = start + 1;
        let source_lazy = self.bytes.get(end) == Some(&b'?');
        self.normalize_repetition(start, end, source_lazy);
        Some(())
    }

    fn normalize_counted_repetition(&mut self) -> Option<()> {
        if let Some(end) = go_counted_repetition_end(self.bytes, self.pos) {
            if self.previous_was_repetition {
                return None;
            }
            let source_lazy = self.bytes.get(end) == Some(&b'?');
            self.normalize_repetition(self.pos, end, source_lazy);
        } else {
            self.flush_pending_flags();
            // Go treats a brace sequence that is not a syntactically complete
            // counted repetition as literal text. Rust accepts a few extra
            // forms, most notably counts with leading zeroes, so quote the
            // opening brace before handing the expression to Rust.
            let atom_start = self.pattern.len();
            self.pattern.push_str(r"\x{7B}");
            self.pos += 1;
            self.last_atom_start = Some(atom_start);
            self.transparent_separator = false;
            self.transparent_requires_group = false;
            self.previous_was_repetition = false;
        }
        Some(())
    }

    fn normalize_repetition(&mut self, start: usize, end: usize, source_lazy: bool) {
        let output_lazy = if self.transparent_separator {
            if self.transparent_requires_group {
                if let Some(atom_start) = self.last_atom_start {
                    let atom = self.pattern.split_off(atom_start);
                    self.pattern.push_str("(?:");
                    self.pattern.push_str(&atom);
                    self.pattern.push(')');
                    self.last_atom_start = Some(atom_start);
                }
            }
            let parser_flags = self.pending_flag_base.unwrap_or(self.flags);
            source_lazy ^ parser_flags[UNGREEDY_FLAG] ^ self.flags[UNGREEDY_FLAG]
        } else {
            self.flush_pending_flags();
            source_lazy
        };

        self.pattern.push_str(&self.source[start..end]);
        if output_lazy {
            self.pattern.push('?');
        }
        self.pos = end + usize::from(source_lazy);
        self.transparent_separator = false;
        self.transparent_requires_group = false;
        self.previous_was_repetition = true;
    }

    fn normalize_quoted_literal(&mut self) -> Option<()> {
        let literal_start = self.pos + 2;
        let (literal_end, next) = match self.source[literal_start..].find("\\E") {
            Some(relative) => {
                let end = literal_start + relative;
                (end, end + 2)
            }
            None => (self.source.len(), self.source.len()),
        };
        if literal_end == literal_start {
            if !self.transparent_separator {
                self.transparent_requires_group = self.previous_was_repetition;
            }
            self.transparent_separator = true;
        } else {
            self.flush_pending_flags();
            for rune in self.source[literal_start..literal_end].chars() {
                let atom_start = self.pattern.len();
                push_pattern_rune(&mut self.pattern, u32::from(rune))?;
                self.last_atom_start = Some(atom_start);
            }
            self.transparent_separator = false;
            self.transparent_requires_group = false;
        }
        self.pos = next;
        self.previous_was_repetition = false;
        Some(())
    }

    fn normalize_unicode_class(&mut self) -> Option<()> {
        let mut negated = self.bytes[self.pos + 1] == b'P';
        let name_start = self.pos + 2;
        if self.bytes.get(name_start) == Some(&b'{') {
            let relative_end = self.bytes[name_start + 1..]
                .iter()
                .position(|&byte| byte == b'}')?;
            let name_end = name_start + 1 + relative_end;
            let mut name = &self.source[name_start + 1..name_end];
            if let Some(rest) = name.strip_prefix('^') {
                negated = !negated;
                name = rest;
            }
            if name.is_empty() {
                return None;
            }
            if !go_unicode_class_name_supported(name) {
                return None;
            }
            if go_unicode_class_is_surrogate(name) {
                if negated {
                    push_full_scalar_class(&mut self.pattern);
                } else {
                    push_empty_class(&mut self.pattern);
                }
                self.pos = name_end + 1;
                return Some(());
            }
            self.pattern.push('\\');
            self.pattern.push(if negated { 'P' } else { 'p' });
            self.pattern.push('{');
            self.pattern.push_str(name);
            self.pattern.push('}');
            self.pos = name_end + 1;
            return Some(());
        }

        let name = self.source[name_start..].chars().next()?;
        if !go_unicode_class_name_supported(&self.source[name_start..name_start + name.len_utf8()])
        {
            return None;
        }
        self.pattern.push('\\');
        self.pattern.push(if negated { 'P' } else { 'p' });
        self.pattern.push(name);
        self.pos = name_start + name.len_utf8();
        Some(())
    }

    fn normalize_class(&mut self) -> Option<()> {
        self.flush_pending_flags();
        let class_start = self.pattern.len();
        self.pattern.push('[');
        self.pos += 1;
        if self.bytes.get(self.pos) == Some(&b'^') {
            self.pattern.push('^');
            self.pos += 1;
        }

        let mut first = true;
        loop {
            let byte = *self.bytes.get(self.pos)?;
            if byte == b']' && !first {
                self.pattern.push(']');
                self.pos += 1;
                self.last_atom_start = Some(class_start);
                self.transparent_separator = false;
                self.transparent_requires_group = false;
                self.previous_was_repetition = false;
                return Some(());
            }
            first = false;

            if byte == b'[' && self.bytes.get(self.pos + 1) == Some(&b':') {
                if let Some(relative_end) = self.source[self.pos + 2..].find(":]") {
                    let end = self.pos + 2 + relative_end + 2;
                    self.pattern.push_str(&self.source[self.pos..end]);
                    self.pos = end;
                    continue;
                }
            }
            if byte == b'\\' {
                match self.bytes.get(self.pos + 1).copied()? {
                    escaped @ (b'd' | b'D' | b's' | b'S' | b'w' | b'W') => {
                        push_perl_class(&mut self.pattern, escaped);
                        self.pos += 2;
                        continue;
                    }
                    b'p' | b'P' => {
                        self.normalize_unicode_class()?;
                        continue;
                    }
                    _ => {}
                }
            }

            let (lower, after_lower) = parse_class_rune(self.source, self.pos)?;
            self.pos = after_lower;
            if self.bytes.get(self.pos) == Some(&b'-')
                && self
                    .bytes
                    .get(self.pos + 1)
                    .is_some_and(|&next| next != b']')
            {
                let (upper, after_upper) = parse_class_rune(self.source, self.pos + 1)?;
                if upper < lower {
                    return None;
                }
                push_class_range(&mut self.pattern, lower, upper)?;
                self.pos = after_upper;
            } else {
                push_class_rune(&mut self.pattern, lower)?;
            }
        }
    }
}

fn go_counted_repetition_end(bytes: &[u8], start: usize) -> Option<usize> {
    debug_assert_eq!(bytes.get(start), Some(&b'{'));
    let mut cursor = go_decimal_end(bytes, start + 1)?;
    if bytes.get(cursor) == Some(&b',') {
        cursor += 1;
        if bytes.get(cursor) != Some(&b'}') {
            cursor = go_decimal_end(bytes, cursor)?;
        }
    }
    if bytes.get(cursor) != Some(&b'}') {
        return None;
    }
    Some(cursor + 1)
}

fn go_decimal_end(bytes: &[u8], start: usize) -> Option<usize> {
    let first = *bytes.get(start)?;
    if !first.is_ascii_digit() {
        return None;
    }
    if first == b'0' && bytes.get(start + 1).is_some_and(u8::is_ascii_digit) {
        return None;
    }

    let mut cursor = start;
    while let Some(&digit) = bytes.get(cursor) {
        if !digit.is_ascii_digit() {
            break;
        }
        cursor += 1;
    }
    Some(cursor)
}

fn canonical_unicode_class_name(name: &str) -> Option<String> {
    let mut canonical = String::with_capacity(name.len());
    for byte in name.bytes() {
        match byte {
            b'_' | b'-' | b' ' => {}
            b'A'..=b'Z' => canonical.push(char::from(byte.to_ascii_lowercase())),
            b'a'..=b'z' | b'0'..=b'9' => canonical.push(char::from(byte)),
            _ => return None,
        }
    }
    (!canonical.is_empty()).then_some(canonical)
}

fn go_unicode_class_name_supported(name: &str) -> bool {
    let Some(canonical) = canonical_unicode_class_name(name) else {
        return false;
    };

    // Go accepts general-category keys, the aliases exposed by package
    // unicode, and three regexp-specific groups. Keep this explicit so a
    // transitive Cargo feature cannot silently widen Volang's syntax to every
    // binary Unicode property supported by the Rust engine.
    if matches!(
        canonical.as_str(),
        "any"
            | "assigned"
            | "ascii"
            | "c"
            | "cc"
            | "cf"
            | "cn"
            | "co"
            | "cs"
            | "l"
            | "lc"
            | "ll"
            | "lm"
            | "lo"
            | "lt"
            | "lu"
            | "m"
            | "mc"
            | "me"
            | "mn"
            | "n"
            | "nd"
            | "nl"
            | "no"
            | "p"
            | "pc"
            | "pd"
            | "pe"
            | "pf"
            | "pi"
            | "po"
            | "ps"
            | "s"
            | "sc"
            | "sk"
            | "sm"
            | "so"
            | "z"
            | "zl"
            | "zp"
            | "zs"
            | "casedletter"
            | "closepunctuation"
            | "combiningmark"
            | "connectorpunctuation"
            | "control"
            | "currencysymbol"
            | "dashpunctuation"
            | "decimalnumber"
            | "enclosingmark"
            | "finalpunctuation"
            | "format"
            | "initialpunctuation"
            | "letter"
            | "letternumber"
            | "lineseparator"
            | "lowercaseletter"
            | "mark"
            | "mathsymbol"
            | "modifierletter"
            | "modifiersymbol"
            | "nonspacingmark"
            | "number"
            | "openpunctuation"
            | "other"
            | "otherletter"
            | "othernumber"
            | "otherpunctuation"
            | "othersymbol"
            | "paragraphseparator"
            | "privateuse"
            | "punctuation"
            | "separator"
            | "spaceseparator"
            | "spacingmark"
            | "surrogate"
            | "symbol"
            | "titlecaseletter"
            | "unassigned"
            | "uppercaseletter"
            | "cntrl"
            | "digit"
            | "punct"
    ) {
        return true;
    }

    let Some(script) = PropertyParser::<Script>::new().get_loose(name) else {
        return false;
    };
    let Some(long_name) = PropertyNamesLong::<Script>::new().get(script) else {
        return false;
    };
    // Go 1.26 canonicalizes the lookup name before consulting unicode.Scripts,
    // whose multi-word keys retain underscores. Consequently only single-word
    // script names are currently reachable; mirror that observable grammar.
    !long_name.contains('_')
        && canonical_unicode_class_name(long_name).as_deref() == Some(canonical.as_str())
}

fn go_unicode_class_is_surrogate(name: &str) -> bool {
    canonical_unicode_class_name(name)
        .is_some_and(|name| matches!(name.as_str(), "cs" | "surrogate"))
}

fn push_perl_class(pattern: &mut String, escaped: u8) {
    let class = match escaped {
        b'd' => r"[\x{30}-\x{39}]",
        b'D' => r"[^\x{30}-\x{39}]",
        b's' => r"[\x{9}\x{A}\x{C}\x{D}\x{20}]",
        b'S' => r"[^\x{9}\x{A}\x{C}\x{D}\x{20}]",
        b'w' => r"[\x{30}-\x{39}\x{41}-\x{5A}\x{5F}\x{61}-\x{7A}]",
        b'W' => r"[^\x{30}-\x{39}\x{41}-\x{5A}\x{5F}\x{61}-\x{7A}]",
        _ => unreachable!("caller only passes a Go Perl-class escape"),
    };
    pattern.push_str(class);
}

fn parse_literal_escape(source: &str, start: usize) -> Option<(u32, usize)> {
    let bytes = source.as_bytes();
    let escaped = *bytes.get(start + 1)?;
    let simple = match escaped {
        b'a' => Some(0x07),
        b'f' => Some(0x0C),
        b'n' => Some(0x0A),
        b'r' => Some(0x0D),
        b't' => Some(0x09),
        b'v' => Some(0x0B),
        _ => None,
    };
    if let Some(rune) = simple {
        return Some((rune, start + 2));
    }
    if escaped == b'x' {
        return parse_hex_escape(source, start);
    }
    if escaped.is_ascii_digit() {
        if escaped > b'7' {
            return None;
        }
        if escaped != b'0'
            && !bytes
                .get(start + 2)
                .is_some_and(|next| (b'0'..=b'7').contains(next))
        {
            // A lone non-zero digit denotes an unsupported backreference in
            // Go rather than a one-digit octal escape.
            return None;
        }
        let mut value = u32::from(escaped - b'0');
        let mut end = start + 2;
        for _ in 1..3 {
            let Some(&digit) = bytes.get(end) else {
                break;
            };
            if !(b'0'..=b'7').contains(&digit) {
                break;
            }
            value = value * 8 + u32::from(digit - b'0');
            end += 1;
        }
        return Some((value, end));
    }
    if escaped.is_ascii() && !escaped.is_ascii_alphanumeric() {
        return Some((u32::from(escaped), start + 2));
    }
    None
}

fn parse_hex_escape(source: &str, start: usize) -> Option<(u32, usize)> {
    let bytes = source.as_bytes();
    let mut cursor = start + 2;
    if bytes.get(cursor) == Some(&b'{') {
        cursor += 1;
        let digit_start = cursor;
        let mut value = 0_u32;
        while let Some(&digit) = bytes.get(cursor) {
            if digit == b'}' {
                if cursor == digit_start || value > 0x10_FFFF {
                    return None;
                }
                return Some((value, cursor + 1));
            }
            let digit = hex_value(digit)?;
            value = value.checked_mul(16)?.checked_add(digit)?;
            if value > 0x10_FFFF {
                return None;
            }
            cursor += 1;
        }
        return None;
    }

    let high = hex_value(*bytes.get(cursor)?)?;
    let low = hex_value(*bytes.get(cursor + 1)?)?;
    Some((high * 16 + low, cursor + 2))
}

fn hex_value(byte: u8) -> Option<u32> {
    match byte {
        b'0'..=b'9' => Some(u32::from(byte - b'0')),
        b'a'..=b'f' => Some(u32::from(byte - b'a') + 10),
        b'A'..=b'F' => Some(u32::from(byte - b'A') + 10),
        _ => None,
    }
}

fn parse_class_rune(source: &str, start: usize) -> Option<(u32, usize)> {
    if source.as_bytes().get(start) == Some(&b'\\') {
        return parse_literal_escape(source, start);
    }
    let rune = source[start..].chars().next()?;
    Some((u32::from(rune), start + rune.len_utf8()))
}

fn push_pattern_rune(pattern: &mut String, rune: u32) -> Option<()> {
    if is_surrogate(rune) {
        push_empty_class(pattern);
        return Some(());
    }
    char::from_u32(rune)?;
    write!(pattern, "\\x{{{rune:X}}}").ok()
}

fn push_class_rune(pattern: &mut String, rune: u32) -> Option<()> {
    push_pattern_rune(pattern, rune)
}

fn push_class_range(pattern: &mut String, lower: u32, upper: u32) -> Option<()> {
    let lower = if is_surrogate(lower) { 0xE000 } else { lower };
    let upper = if is_surrogate(upper) { 0xD7FF } else { upper };
    if lower > upper {
        push_empty_class(pattern);
        return Some(());
    }
    char::from_u32(lower)?;
    char::from_u32(upper)?;
    write!(pattern, "\\x{{{lower:X}}}-\\x{{{upper:X}}}").ok()
}

fn is_surrogate(rune: u32) -> bool {
    (0xD800..=0xDFFF).contains(&rune)
}

fn push_empty_class(pattern: &mut String) {
    // Unicode scalar input can match neither side of this intersection. The
    // normalizer quotes all user-provided class punctuation, so any set
    // operator reaching the Rust parser is necessarily this internal atom.
    pattern.push_str(r"[\x{0}&&\x{1}]");
}

fn push_full_scalar_class(pattern: &mut String) {
    pattern.push_str(r"[\x{0}-\x{10FFFF}]");
}

struct GoAstValidator {
    repeat_budgets: Vec<u32>,
}

impl GoAstValidator {
    fn new() -> Self {
        Self {
            repeat_budgets: Vec::new(),
        }
    }

    fn repetition_factor(repetition: &ast::Repetition) -> Option<u32> {
        let ast::RepetitionKind::Range(ref range) = repetition.op.kind else {
            return None;
        };
        Some(match *range {
            ast::RepetitionRange::Exactly(count) => count,
            ast::RepetitionRange::AtLeast(minimum) => minimum,
            ast::RepetitionRange::Bounded(_, maximum) => maximum,
        })
    }
}

impl ast::Visitor for GoAstValidator {
    type Output = ();
    type Err = ();

    fn finish(self) -> Result<Self::Output, Self::Err> {
        Ok(())
    }

    fn visit_pre(&mut self, ast: &Ast) -> Result<(), Self::Err> {
        let Ast::Repetition(repetition) = ast else {
            return Ok(());
        };
        let Some(factor) = Self::repetition_factor(repetition) else {
            return Ok(());
        };
        let budget = self
            .repeat_budgets
            .last()
            .copied()
            .unwrap_or(GO_MAX_REPEAT_PRODUCT);
        if factor > budget {
            return Err(());
        }
        self.repeat_budgets
            .push(if factor == 0 { budget } else { budget / factor });
        Ok(())
    }

    fn visit_post(&mut self, ast: &Ast) -> Result<(), Self::Err> {
        if let Ast::Repetition(repetition) = ast {
            if Self::repetition_factor(repetition).is_some() {
                self.repeat_budgets
                    .pop()
                    .expect("repeat pre/post visits balance");
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn re(pattern: &str) -> CompiledRegex {
        compile(pattern.as_bytes()).unwrap_or_else(|| panic!("pattern should compile: {pattern:?}"))
    }

    #[test]
    fn perl_classes_and_word_boundaries_are_ascii() {
        assert!(re(r"\d").is_match("3"));
        assert!(!re(r"\d").is_match("٣"));
        assert!(re(r"\w").is_match("_"));
        assert!(!re(r"\w").is_match("é"));
        assert!(re(r"\s").is_match("\t"));
        assert!(!re(r"\s").is_match("\u{000B}"));
        assert!(!re(r"\s").is_match("\u{00A0}"));
        assert!(!re(r"\b").is_match("é"));
        assert!(re(r"\b_").is_match("_"));

        assert!(re(r"[^\D]").is_match("7"));
        assert!(!re(r"[^\D]").is_match("٣"));
        assert!(re(r"[\D0]").is_match("0"));
        assert!(re(r"[\D0]").is_match("٣"));
    }

    #[test]
    fn anchors_dot_case_fold_and_posix_classes_match_go() {
        assert!(!re(".").is_match("\n"));
        assert!(re("(?s).").is_match("\n"));
        assert!(!re("^x$").is_match("x\n"));
        assert!(re("(?m)^x$").is_match("a\nx\nb"));

        assert!(re("(?i)k").is_match("K"));
        assert!(re("(?i)s").is_match("ſ"));
        assert!(re("[[:alpha:]]").is_match("a"));
        assert!(!re("[[:alpha:]]").is_match("é"));
        assert!(re("[[:space:]]").is_match("\u{000B}"));
        assert!(re("[[:cntrl:]]").is_match("\u{007F}"));
    }

    #[test]
    fn go_unicode_categories_scripts_and_special_groups_compile() {
        assert!(re(r"\pL").is_match("界"));
        assert!(re(r"\p{Greek}").is_match("λ"));
        assert!(re(r"\p{ASCII}").is_match("A"));
        assert!(re(r"\p{Any}").is_match("🙂"));
        assert!(re(r"\p{Assigned}").is_match("A"));
        assert!(re(r"\p{LC}").is_match("A"));
        assert!(re(r"\p{lower-case letter}").is_match("a"));
        assert!(compile(br"\p{Alphabetic}").is_none());
        assert!(compile(br"\p{Latn}").is_none());
        assert!(compile(br"\p{Canadian_Aboriginal}").is_none());
    }

    #[test]
    fn go_escapes_are_normalized_and_rust_extensions_are_rejected() {
        assert!(re(r"\141").is_match("a"));
        assert!(re(r"\12").is_match("\n"));
        assert!(re(r"\x7F").is_match("\u{007F}"));
        assert!(re(r"\Q[a].\E").is_match("[a]."));
        let quoted_syntax = re(r"\Q(?P<x>\d[&&])\E");
        assert!(quoted_syntax.is_match(r"(?P<x>\d[&&])"));
        assert_eq!(quoted_syntax.capture_names_go(), &[None]);
        assert!(re(r"\<\>").is_match("<>"));

        for invalid in [
            r"\1",
            r"\8",
            r"\u0061",
            r"\U00000061",
            r"\C",
            r"(?x)a",
            r"(?u)a",
            r"(?R)a",
            r"(?=a)",
            r"(?<=a)",
        ] {
            assert!(
                compile(invalid.as_bytes()).is_none(),
                "accepted {invalid:?}"
            );
        }
    }

    #[test]
    fn surrogate_escapes_compile_as_unmatchable_code_points() {
        assert!(!re(r"\x{D800}").is_match(""));
        assert!(!re(r"[\x{D800}]").is_match("A"));
        assert!(re(r"[^\x{D800}]").is_match("A"));
        assert!(re(r"[\x{D7FF}-\x{D800}]").is_match("\u{D7FF}"));
        assert!(re(r"[\x{DFFF}-\x{E000}]").is_match("\u{E000}"));
        assert!(!re(r"\p{Cs}").is_match("�"));
        assert!(re(r"\P{Cs}").is_match("A"));
    }

    #[test]
    fn repetition_limits_include_nested_products() {
        assert!(compile(b"a{1000}").is_some());
        assert!(compile(b"a{1001}").is_none());
        assert!(compile(b"(a{10}){100}").is_some());
        assert!(compile(b"(a{11}){100}").is_none());

        let leading_zero = re(r"a{01}");
        assert!(leading_zero.is_match("a{01}"));
        assert!(!leading_zero.is_match("a"));
        let leading_zero_bound = re(r"a{1,01}");
        assert!(leading_zero_bound.is_match("a{1,01}"));
    }

    #[test]
    fn named_captures_follow_go_name_and_duplicate_rules() {
        let numeric = re(r"(?P<1>a)");
        assert_eq!(numeric.capture_names_go()[1].as_deref(), Some("1"));

        let duplicate = re(r"(?P<x>a)(?<x>b)");
        assert_eq!(duplicate.capture_names_go()[1].as_deref(), Some("x"));
        assert_eq!(duplicate.capture_names_go()[2].as_deref(), Some("x"));

        assert!(compile("(?P<é>a)".as_bytes()).is_none());
        assert!(compile(b"(?P<>a)").is_none());
    }

    #[test]
    fn rust_class_set_operators_remain_go_literals() {
        let ampersands = re(r"[a&&b]+");
        assert!(ampersands.is_match("&&"));
        assert!(ampersands.is_match("a"));
        assert!(compile(br"[a--b]").is_none());
    }

    #[test]
    fn duplicate_flags_are_canonicalized_with_go_last_clear_semantics() {
        assert!(re("(?ii)a").is_match("A"));
        assert!(!re("(?i-i)a").is_match("A"));
        assert!(re("(?)").is_match(""));
        assert!(compile(b"(?-)").is_none());
    }

    #[test]
    fn repetitions_after_zero_width_syntax_follow_go() {
        assert!(compile(b"(?i){2}").is_none());

        let after_flags = re("a(?i){2}");
        assert!(!after_flags.is_match("a"));
        assert!(after_flags.is_match("aa"));
        assert!(re("a(?i)*").is_match(""));
        assert!(re("a(?:){2}").is_match("a"));
        assert!(re("^{2}").is_match(""));
        assert!(re(r"\b{2}").is_match("a"));
        assert!(re(r"a\Q\E{2}").is_match("aa"));
        assert!(compile(br"\Q\E{2}").is_none());
        assert!(compile(b"a{0}{2}").is_none());
        assert!(compile(b"a**").is_none());

        assert!(re("a*(?i)*").is_match("aaa"));
        assert!(re(r"a*\Q\E*").is_match("aaa"));
        let flags_after_repeat = re("a(?i){2}B");
        assert!(flags_after_repeat.is_match("aab"));
        assert!(!flags_after_repeat.is_match("aAB"));
        assert_eq!(re("a(?U)*a").find("aaa").unwrap().as_str(), "a");
        assert_eq!(re("a(?U)*?a").find("aaa").unwrap().as_str(), "aaa");
        assert_eq!(re("(?U)a(?-U)*?a").find("aaa").unwrap().as_str(), "a");

        assert!(re("a(?i){2}|b").is_match("B"));
        let branch_scoped_flags = re("(a(?i)|b)c");
        assert!(branch_scoped_flags.is_match("Bc"));
        assert!(!branch_scoped_flags.is_match("bC"));
    }
}
