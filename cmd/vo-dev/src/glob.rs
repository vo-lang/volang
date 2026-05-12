pub(crate) fn path_matches(path: &str, pattern: &str) -> bool {
    let path_parts: Vec<&str> = path.split('/').collect();
    let pattern_parts: Vec<&str> = pattern.split('/').collect();
    match_parts(&path_parts, &pattern_parts)
}

fn match_parts(path: &[&str], pattern: &[&str]) -> bool {
    if pattern.is_empty() {
        return path.is_empty();
    }
    if pattern[0] == "**" {
        return match_parts(path, &pattern[1..])
            || (!path.is_empty() && match_parts(&path[1..], pattern));
    }
    if path.is_empty() {
        return false;
    }
    segment_matches(path[0], pattern[0]) && match_parts(&path[1..], &pattern[1..])
}

fn segment_matches(text: &str, pattern: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    if !pattern.contains('*') {
        return text == pattern;
    }
    let mut remaining = text;
    let mut first = true;
    for part in pattern.split('*') {
        if part.is_empty() {
            continue;
        }
        if first && !pattern.starts_with('*') {
            if !remaining.starts_with(part) {
                return false;
            }
            remaining = &remaining[part.len()..];
        } else if let Some(index) = remaining.find(part) {
            remaining = &remaining[index + part.len()..];
        } else {
            return false;
        }
        first = false;
    }
    pattern.ends_with('*') || remaining.is_empty()
}

#[cfg(test)]
mod tests {
    use super::path_matches;

    #[test]
    fn matches_recursive_prefix_globs() {
        assert!(path_matches(
            "lang/crates/vo-web/src/lib.rs",
            "lang/crates/**"
        ));
        assert!(path_matches("lang/crates", "lang/crates/**"));
        assert!(!path_matches("cmd/vo/src/main.rs", "lang/crates/**"));
    }

    #[test]
    fn matches_recursive_suffix_globs() {
        assert!(path_matches("Cargo.toml", "Cargo.toml"));
        assert!(path_matches("cmd/vo/Cargo.toml", "**/Cargo.toml"));
        assert!(path_matches(
            "lang/crates/vo-web/Cargo.toml",
            "**/Cargo.toml"
        ));
        assert!(!path_matches("Cargo.lock", "**/Cargo.toml"));
    }

    #[test]
    fn matches_segment_wildcards() {
        assert!(path_matches(
            "apps/studio/vite.config.ts",
            "apps/studio/vite.config.*"
        ));
        assert!(path_matches(
            "apps/studio/tsconfig.app.json",
            "apps/studio/tsconfig*.json"
        ));
        assert!(!path_matches(
            "apps/studio/src/tsconfig.app.json",
            "apps/studio/tsconfig*.json"
        ));
    }
}
