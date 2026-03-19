pub mod modfile;
pub mod lockfile;
pub mod workfile;
pub mod manifest;

/// Validate a Git commit hash: exactly 40 lowercase hex characters.
/// Returns `Ok(())` or an error message string for the caller to wrap.
pub(crate) fn validate_commit_hash(commit: &str) -> Result<(), String> {
    if commit.len() != 40 {
        return Err(format!(
            "commit must be 40-char hex, got {} chars",
            commit.len()
        ));
    }
    if !commit.chars().all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase()) {
        return Err("commit must be lowercase hex".into());
    }
    Ok(())
}
