//! Workspace file (`vo.work`) parser and resolver.
//!
//! A `vo.work` file eliminates the need for `replace` directives in individual
//! `vo.mod` files during multi-repo development.  Place `vo.work` in any
//! ancestor directory of your projects; the compiler discovers it automatically
//! by walking up from the project root.
//!
//! # Format
//!
//! ```text
//! vo 0.1
//!
//! use ./vopack
//! use ./voplay
//! use ./vogui
//! ```
//!
//! Each `use <path>` entry points to a local directory that contains a `vo.mod`
//! file.  The module name is read from that `vo.mod` and becomes the key in the
//! replace map, equivalent to writing `replace <module> => <path>` in every
//! `vo.mod` in the workspace.
//!
//! Paths are resolved relative to the directory containing `vo.work`.
//! Absolute paths are also accepted.
//!
//! `vo.mod` `replace` directives take precedence over `vo.work` entries, so a
//! project can still pin a specific local path for a single dependency without
//! touching the workspace file.

use std::collections::HashMap;
use std::fmt;
use std::path::{Path, PathBuf};
use std::env;

/// A parsed `vo.work` workspace file.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct WorkFile {
    /// Paths declared with `use`, as written in the file (possibly relative).
    pub uses: Vec<String>,
}

impl WorkFile {
    /// Parse a `vo.work` file from its string content.
    ///
    /// Unknown directives and blank lines are silently ignored so the format
    /// can be extended without breaking older tooling.
    pub fn parse(content: &str) -> Self {
        let mut uses = Vec::new();
        for line in content.lines() {
            let t = line.trim();
            if let Some(path) = t.strip_prefix("use ") {
                let p = path.trim();
                if !p.is_empty() {
                    uses.push(p.to_string());
                }
            }
        }
        WorkFile { uses }
    }

    /// Add a `use` entry if it is not already present.
    pub fn add_use(&mut self, path: &str) {
        let p = path.trim();
        if !p.is_empty() && !self.uses.iter().any(|u| u == p) {
            self.uses.push(p.to_string());
        }
    }
}

impl fmt::Display for WorkFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "vo 0.1")?;
        if !self.uses.is_empty() {
            writeln!(f)?;
            for u in &self.uses {
                writeln!(f, "use {}", u)?;
            }
        }
        Ok(())
    }
}

/// Walk up the directory tree from `start`, looking for a `vo.work` file.
///
/// Returns `(work_dir, WorkFile)` where `work_dir` is the directory that
/// contains `vo.work`, or `None` if no such file is found before the root.
pub fn find_work_file(start: &Path) -> Option<(PathBuf, WorkFile)> {
    let mut dir = start.canonicalize().ok()?;
    loop {
        let candidate = dir.join("vo.work");
        if candidate.is_file() {
            if let Ok(content) = std::fs::read_to_string(&candidate) {
                return Some((dir, WorkFile::parse(&content)));
            }
        }
        match dir.parent() {
            Some(p) => dir = p.to_path_buf(),
            None => return None,
        }
    }
}

/// Read the `module` declaration from `<dir>/vo.mod`, returning the module
/// path string (e.g. `"github.com/vo-lang/vopack"`) or `None` on failure.
fn read_module_path(dir: &Path) -> Option<String> {
    let content = std::fs::read_to_string(dir.join("vo.mod")).ok()?;
    for line in content.lines() {
        if let Some(rest) = line.trim().strip_prefix("module ") {
            let m = rest.trim();
            if !m.is_empty() {
                return Some(m.to_string());
            }
        }
    }
    None
}

/// Find the nearest `vo.work` ancestor of `project_root` and build a
/// `module_path → absolute_local_dir` replace map from its `use` entries.
///
/// The `project_root` directory itself is skipped so a module never
/// replaces itself via the workspace file.
///
/// Set `VOWORK=off` in the environment to disable workspace mode entirely,
/// which makes the compiler behave as if no `vo.work` exists (useful for
/// testing published/remote behaviour locally).
///
/// Prints a warning to stderr for any `use` entry whose directory does not
/// exist or whose `vo.mod` cannot be read, so typos are surfaced immediately
/// rather than silently falling back to a remote fetch.
///
/// Returns an empty map when no `vo.work` is found or when workspace mode
/// has been disabled via the environment variable.
pub fn find_workspace_replaces(project_root: &Path) -> HashMap<String, PathBuf> {
    // Allow opting out of workspace mode without editing vo.work.
    if env::var("VOWORK").as_deref() == Ok("off") {
        return HashMap::new();
    }

    let mut map = HashMap::new();
    let Some((work_dir, work)) = find_work_file(project_root) else {
        return map;
    };
    let canonical_root = project_root.canonicalize().unwrap_or_else(|_| project_root.to_path_buf());
    for use_path in &work.uses {
        let local = Path::new(use_path.as_str());
        let abs = if local.is_absolute() {
            local.to_path_buf()
        } else {
            work_dir.join(local)
        };
        let abs = match abs.canonicalize() {
            Ok(p) => p,
            Err(_) => {
                eprintln!(
                    "vo.work: warning: 'use {}' does not resolve to an existing directory ({})",
                    use_path,
                    abs.display()
                );
                continue;
            }
        };
        // Never self-replace: skip if the use path resolves to the project root.
        if abs == canonical_root {
            continue;
        }
        match read_module_path(&abs) {
            Some(module) => {
                map.insert(module, abs);
            }
            None => {
                eprintln!(
                    "vo.work: warning: 'use {}' — no valid module declaration found in {}/vo.mod",
                    use_path,
                    abs.display()
                );
            }
        }
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_parse_empty() {
        let wf = WorkFile::parse("");
        assert!(wf.uses.is_empty());
    }

    #[test]
    fn test_parse_basic() {
        let src = "vo 0.1\n\nuse ./vopack\nuse ./voplay\n";
        let wf = WorkFile::parse(src);
        assert_eq!(wf.uses, vec!["./vopack", "./voplay"]);
    }

    #[test]
    fn test_parse_ignores_unknown_directives() {
        let src = "vo 0.1\nunknown directive\nuse ./foo\n# comment\nuse ./bar\n";
        let wf = WorkFile::parse(src);
        assert_eq!(wf.uses, vec!["./foo", "./bar"]);
    }

    #[test]
    fn test_display_roundtrip() {
        let src = "vo 0.1\n\nuse ./vopack\nuse ./voplay\n";
        let wf = WorkFile::parse(src);
        assert_eq!(wf.to_string(), src);
    }

    #[test]
    fn test_add_use_deduplicates() {
        let mut wf = WorkFile::parse("vo 0.1\nuse ./foo\n");
        wf.add_use("./bar");
        wf.add_use("./foo");  // duplicate, must not be added
        assert_eq!(wf.uses, vec!["./foo", "./bar"]);
    }

    #[test]
    fn test_parse_trims_whitespace() {
        let src = "  use   ./vopack  \n";
        let wf = WorkFile::parse(src);
        assert_eq!(wf.uses, vec!["./vopack"]);
    }

    #[test]
    fn test_find_work_file_walks_up() {
        let temp = TempDir::new().unwrap();
        let work_path = temp.path().join("vo.work");
        fs::write(&work_path, "vo 0.1\nuse ./foo\n").unwrap();

        let nested = temp.path().join("a").join("b").join("c");
        fs::create_dir_all(&nested).unwrap();

        let (found_dir, wf) = find_work_file(&nested).expect("should find vo.work");
        assert_eq!(found_dir, temp.path().canonicalize().unwrap());
        assert_eq!(wf.uses, vec!["./foo"]);
    }

    #[test]
    fn test_find_workspace_replaces_builds_map() {
        let temp = TempDir::new().unwrap();

        // Create workspace vo.work
        fs::write(temp.path().join("vo.work"), "vo 0.1\nuse ./libfoo\nuse ./libbar\n").unwrap();

        // Create libfoo with vo.mod
        let libfoo = temp.path().join("libfoo");
        fs::create_dir_all(&libfoo).unwrap();
        fs::write(libfoo.join("vo.mod"), "module github.com/acme/foo\n").unwrap();

        // Create libbar with vo.mod
        let libbar = temp.path().join("libbar");
        fs::create_dir_all(&libbar).unwrap();
        fs::write(libbar.join("vo.mod"), "module github.com/acme/bar\n").unwrap();

        // Project root is a subdirectory (not libfoo or libbar)
        let project = temp.path().join("myproject");
        fs::create_dir_all(&project).unwrap();
        fs::write(project.join("vo.mod"), "module github.com/acme/myproject\n").unwrap();

        let replaces = find_workspace_replaces(&project);
        assert_eq!(replaces.len(), 2);
        assert_eq!(replaces["github.com/acme/foo"], libfoo.canonicalize().unwrap());
        assert_eq!(replaces["github.com/acme/bar"], libbar.canonicalize().unwrap());
    }

    #[test]
    fn test_find_workspace_replaces_skips_self() {
        let temp = TempDir::new().unwrap();

        // vo.work and project are in the same directory
        fs::write(temp.path().join("vo.work"), "vo 0.1\nuse ./mylib\n").unwrap();
        let mylib = temp.path().join("mylib");
        fs::create_dir_all(&mylib).unwrap();
        fs::write(mylib.join("vo.mod"), "module github.com/acme/mylib\n").unwrap();

        // When project_root IS mylib, it must not self-replace.
        let replaces = find_workspace_replaces(&mylib);
        assert!(replaces.is_empty(), "project root must not self-replace via workspace");
    }
}
