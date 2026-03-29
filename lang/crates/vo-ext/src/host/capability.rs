//! Host capability queries.

/// Check whether the host provides a named capability.
///
/// Returns `false` if no host bridge is installed (e.g. the extension is
/// loaded in a context that does not use a host bridge).
pub fn has(name: &str) -> bool {
    super::with_bridge(|b| b.capabilities.iter().any(|c| c == name)).unwrap_or(false)
}
