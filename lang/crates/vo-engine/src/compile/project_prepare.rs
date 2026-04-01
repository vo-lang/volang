use std::collections::HashMap;
use std::path::{Path, PathBuf};

use vo_analysis::vfs::{ModSource, PackageResolverMixed, ReplacingResolver, StdSource};
use vo_common::vfs::RealFs;
use vo_module::project::ProjectDeps;
use vo_stdlib::EmbeddedStdlib;

pub(super) fn create_resolver(plan: &ProjectDeps, mod_cache: &Path) -> PackageResolverMixed<EmbeddedStdlib, RealFs> {
    PackageResolverMixed {
        std: StdSource::with_fs(EmbeddedStdlib::new()),
        r#mod: ModSource::with_fs(RealFs::new(mod_cache)).with_project_deps(plan),
    }
}

pub(super) fn replacing_resolver(
    plan: &ProjectDeps,
    mod_cache: &Path,
    workspace_replaces: HashMap<String, PathBuf>,
) -> ReplacingResolver<PackageResolverMixed<EmbeddedStdlib, RealFs>> {
    ReplacingResolver::new(create_resolver(plan, mod_cache), workspace_replaces)
}
