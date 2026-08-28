use vo_module::bundle::{BundledSourceFile, BundledSourceRegistry};

const OFFICIAL_UI_SOURCES: &[BundledSourceFile<'static>] = &[
    BundledSourceFile::regular(
        "animation/animation.vo",
        include_bytes!("../../../ui/animation/animation.vo"),
    ),
    BundledSourceFile::regular(
        "assets/assets.vo",
        include_bytes!("../../../ui/assets/assets.vo"),
    ),
    BundledSourceFile::regular(
        "chart/chart.vo",
        include_bytes!("../../../ui/chart/chart.vo"),
    ),
    BundledSourceFile::regular(
        "commands/commands.vo",
        include_bytes!("../../../ui/commands/commands.vo"),
    ),
    BundledSourceFile::regular(
        "desktop/desktop.vo",
        include_bytes!("../../../ui/desktop/desktop.vo"),
    ),
    BundledSourceFile::regular(
        "document/document.vo",
        include_bytes!("../../../ui/document/document.vo"),
    ),
    BundledSourceFile::regular(
        "editor/editor.vo",
        include_bytes!("../../../ui/editor/editor.vo"),
    ),
    BundledSourceFile::regular(
        "forms/forms.vo",
        include_bytes!("../../../ui/forms/forms.vo"),
    ),
    BundledSourceFile::regular(
        "gesture/gesture.vo",
        include_bytes!("../../../ui/gesture/gesture.vo"),
    ),
    BundledSourceFile::regular(
        "graphics/graphics.vo",
        include_bytes!("../../../ui/graphics/graphics.vo"),
    ),
    BundledSourceFile::regular(
        "i18n/core/core.vo",
        include_bytes!("../../../ui/i18n/core/core.vo"),
    ),
    BundledSourceFile::regular("i18n/i18n.vo", include_bytes!("../../../ui/i18n/i18n.vo")),
    BundledSourceFile::regular(
        "language/language.vo",
        include_bytes!("../../../ui/language/language.vo"),
    ),
    BundledSourceFile::regular(
        "kit/components/components.vo",
        include_bytes!("../../../ui/kit/components/components.vo"),
    ),
    BundledSourceFile::regular(
        "kit/data/data.vo",
        include_bytes!("../../../ui/kit/data/data.vo"),
    ),
    BundledSourceFile::regular(
        "kit/headless/headless.vo",
        include_bytes!("../../../ui/kit/headless/headless.vo"),
    ),
    BundledSourceFile::regular(
        "kit/icons/icons.vo",
        include_bytes!("../../../ui/kit/icons/icons.vo"),
    ),
    BundledSourceFile::regular("kit/kit.vo", include_bytes!("../../../ui/kit/kit.vo")),
    BundledSourceFile::regular(
        "kit/tokens/tokens.vo",
        include_bytes!("../../../ui/kit/tokens/tokens.vo"),
    ),
    BundledSourceFile::regular(
        "motion/motion.vo",
        include_bytes!("../../../ui/motion/motion.vo"),
    ),
    BundledSourceFile::regular(
        "media/media.vo",
        include_bytes!("../../../ui/media/media.vo"),
    ),
    BundledSourceFile::regular(
        "navigation/navigation.vo",
        include_bytes!("../../../ui/navigation/navigation.vo"),
    ),
    BundledSourceFile::regular(
        "observability/observability.vo",
        include_bytes!("../../../ui/observability/observability.vo"),
    ),
    BundledSourceFile::regular(
        "persistence/persistence.vo",
        include_bytes!("../../../ui/persistence/persistence.vo"),
    ),
    BundledSourceFile::regular(
        "platform/platform.vo",
        include_bytes!("../../../ui/platform/platform.vo"),
    ),
    BundledSourceFile::regular(
        "resource/resource.vo",
        include_bytes!("../../../ui/resource/resource.vo"),
    ),
    BundledSourceFile::regular(
        "system/system.vo",
        include_bytes!("../../../ui/system/system.vo"),
    ),
    BundledSourceFile::regular("task/task.vo", include_bytes!("../../../ui/task/task.vo")),
    BundledSourceFile::regular(
        "testing/testing.vo",
        include_bytes!("../../../ui/testing/testing.vo"),
    ),
    BundledSourceFile::regular("web/web.vo", include_bytes!("../../../ui/web/web.vo")),
    BundledSourceFile::regular(
        "web/server/server.vo",
        include_bytes!("../../../ui/web/server/server.vo"),
    ),
    BundledSourceFile::regular(
        "workspace/workspace.vo",
        include_bytes!("../../../ui/workspace/workspace.vo"),
    ),
    BundledSourceFile::regular("ui.vo", include_bytes!("../../../ui/ui.vo")),
    BundledSourceFile::regular("vo.mod", include_bytes!("../../../ui/vo.mod")),
];

pub(super) fn official_ui_registry() -> Result<BundledSourceRegistry, String> {
    let registry = BundledSourceRegistry::new(OFFICIAL_UI_SOURCES)
        .map_err(|error| format!("cannot prepare bundled official UI module: {error}"))?;
    if registry.manifest().version.to_string() != env!("CARGO_PKG_VERSION") {
        return Err(format!(
            "bundled official UI version {} does not match CLI version {}",
            registry.manifest().version,
            env!("CARGO_PKG_VERSION")
        ));
    }
    Ok(registry)
}

pub(super) fn official_ui_source_export(name: &str) -> Option<(&'static str, &'static [u8])> {
    let path = match name {
        "kit/components" => "kit/components/components.vo",
        "kit/data" => "kit/data/data.vo",
        "kit/headless" => "kit/headless/headless.vo",
        "kit/icons" => "kit/icons/icons.vo",
        "kit/tokens" => "kit/tokens/tokens.vo",
        _ => return None,
    };
    OFFICIAL_UI_SOURCES
        .iter()
        .find(|source| source.path == path)
        .map(|source| (source.path, source.bytes))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bundled_official_ui_release_matches_the_cli_and_has_no_external_dependencies() {
        let registry = official_ui_registry().unwrap();
        assert_eq!(registry.manifest().module.as_str(), "github.com/vo-lang/ui");
        assert_eq!(
            registry.manifest().version.to_string(),
            env!("CARGO_PKG_VERSION")
        );
        assert!(registry.manifest().dependencies.is_empty());
    }
}
