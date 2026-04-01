use std::collections::{BTreeMap, BTreeSet};

use crate::identity::ModulePath;
use crate::schema::lockfile::LockedModule;
use crate::version::ExactVersion;
use crate::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleSelection {
    pub module: ModulePath,
    pub version: ExactVersion,
}

impl ModuleSelection {
    pub fn new(module: ModulePath, version: ExactVersion) -> Self {
        Self { module, version }
    }

    pub fn parse(module: &str, version: &str) -> Result<Self, Error> {
        Ok(Self::new(ModulePath::parse(module)?, ExactVersion::parse(version)?))
    }

    pub fn from_locked_module(locked: &LockedModule) -> Self {
        Self::new(locked.path.clone(), locked.version.clone())
    }

    pub fn spec(&self) -> String {
        format!("{}@{}", self.module.as_str(), self.version)
    }
}

#[derive(Debug, Default)]
pub struct ModuleSelectionPlanner {
    stack: Vec<ModuleSelection>,
    visited: BTreeSet<String>,
    selected_versions: BTreeMap<String, String>,
}

impl ModuleSelectionPlanner {
    pub fn new(initial: Vec<ModuleSelection>) -> Result<Self, Error> {
        let mut planner = Self::default();
        planner.push_all(initial)?;
        Ok(planner)
    }

    pub fn push_all<I>(&mut self, selections: I) -> Result<(), Error>
    where
        I: IntoIterator<Item = ModuleSelection>,
    {
        for selection in selections {
            remember_selected_version(&mut self.selected_versions, &selection)?;
            self.stack.push(selection);
        }
        Ok(())
    }

    pub fn next(&mut self) -> Option<ModuleSelection> {
        while let Some(selection) = self.stack.pop() {
            if self.visited.insert(selection.spec()) {
                return Some(selection);
            }
        }
        None
    }
}

pub fn locked_module_selections(locked_modules: &[LockedModule]) -> Vec<ModuleSelection> {
    locked_modules
        .iter()
        .map(ModuleSelection::from_locked_module)
        .collect()
}

pub fn normalize_locked_modules(locked_modules: Vec<LockedModule>) -> Result<Vec<LockedModule>, Error> {
    let mut selected_versions = BTreeMap::new();
    let mut visited = BTreeSet::new();
    let mut normalized = Vec::new();

    for locked in locked_modules {
        let selection = ModuleSelection::from_locked_module(&locked);
        remember_selected_version(&mut selected_versions, &selection)?;
        if !visited.insert(selection.spec()) {
            continue;
        }
        normalized.push(locked);
    }

    Ok(normalized)
}

pub fn walk_module_selection_closure<F>(
    initial: Vec<ModuleSelection>,
    mut dependencies_of: F,
) -> Result<Vec<ModuleSelection>, Error>
where
    F: FnMut(&ModuleSelection) -> Result<Vec<ModuleSelection>, Error>,
{
    let mut planner = ModuleSelectionPlanner::new(initial)?;
    let mut resolved = Vec::new();

    while let Some(selection) = planner.next() {
        let deps = dependencies_of(&selection)?;
        planner.push_all(deps)?;
        resolved.push(selection);
    }

    Ok(resolved)
}

fn remember_selected_version(
    selected_versions: &mut BTreeMap<String, String>,
    selection: &ModuleSelection,
) -> Result<(), Error> {
    let module = selection.module.as_str();
    let version = selection.version.to_string();
    match selected_versions.get(module) {
        Some(existing) if existing != &version => Err(Error::SelectedVersionConflict {
            module: module.to_string(),
            existing: existing.clone(),
            requested: version,
        }),
        Some(_) => Ok(()),
        None => {
            selected_versions.insert(module.to_string(), version);
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::digest::Digest;
    use crate::schema::lockfile::LockedModule;
    use crate::version::ToolchainConstraint;

    fn selection(module: &str, version: &str) -> ModuleSelection {
        ModuleSelection::parse(module, version).unwrap()
    }

    fn locked(module: &str, version: &str) -> LockedModule {
        LockedModule {
            path: ModulePath::parse(module).unwrap(),
            version: ExactVersion::parse(version).unwrap(),
            vo: ToolchainConstraint::parse("^0.1.0").unwrap(),
            commit: "1111111111111111111111111111111111111111".to_string(),
            release_manifest: Digest::parse(
                "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            )
            .unwrap(),
            source: Digest::parse(
                "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            )
            .unwrap(),
            deps: Vec::new(),
            artifacts: Vec::new(),
        }
    }

    #[test]
    fn test_normalize_locked_modules_rejects_conflicting_versions() {
        let err = normalize_locked_modules(vec![
            locked("github.com/acme/lib", "v1.0.0"),
            locked("github.com/acme/lib", "v2.0.0"),
        ])
        .unwrap_err();

        assert!(matches!(err, Error::SelectedVersionConflict { .. }));
    }

    #[test]
    fn test_walk_module_selection_closure_dedupes_revisited_modules() {
        let resolved = walk_module_selection_closure(
            vec![selection("github.com/acme/app", "v1.0.0")],
            |current| match current.module.as_str() {
                "github.com/acme/app" => Ok(vec![
                    selection("github.com/acme/lib", "v1.0.0"),
                    selection("github.com/acme/util", "v1.0.0"),
                ]),
                "github.com/acme/lib" => Ok(vec![selection("github.com/acme/util", "v1.0.0")]),
                _ => Ok(Vec::new()),
            },
        )
        .unwrap();

        let specs = resolved.into_iter().map(|selection| selection.spec()).collect::<Vec<_>>();
        assert_eq!(
            specs,
            vec![
                "github.com/acme/app@v1.0.0",
                "github.com/acme/util@v1.0.0",
                "github.com/acme/lib@v1.0.0",
            ]
        );
    }

    #[test]
    fn test_walk_module_selection_closure_rejects_conflicting_versions() {
        let err = walk_module_selection_closure(
            vec![selection("github.com/acme/app", "v1.0.0")],
            |current| match current.module.as_str() {
                "github.com/acme/app" => Ok(vec![
                    selection("github.com/acme/lib", "v1.0.0"),
                    selection("github.com/acme/lib", "v2.0.0"),
                ]),
                _ => Ok(Vec::new()),
            },
        )
        .unwrap_err();

        assert!(matches!(err, Error::SelectedVersionConflict { .. }));
    }
}
