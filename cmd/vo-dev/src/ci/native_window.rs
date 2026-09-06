//! Bind a native presentation and semantic interaction log to its declared script.
use super::process::TestCounts;
use anyhow::{bail, Result};
use std::collections::BTreeMap;

const PREFIX: &str = "[VO:UI:CERTIFY] ";
const ASSERTED: &str = "semantic interaction script and final assertions passed";
const PRESENTED: &str = "certified native frame reached the presentation boundary";

fn script(env: &BTreeMap<String, String>) -> Result<(Vec<String>, u64)> {
    if env
        .get("VO_UI_AUTOMATION_EXIT_AFTER_FRAMES")
        .map(String::as_str)
        != Some("1")
    {
        bail!("native window evidence requires exactly one presented frame");
    }
    let mut messages = Vec::new();
    let assertions = match (
        env.get("VO_UI_AUTOMATION_CLICKS"),
        env.get("VO_UI_AUTOMATION_EXPECT_TEXT"),
    ) {
        (None, None) => 0,
        (Some(clicks), Some(text)) => {
            if clicks.split('|').chain(text.split('|')).any(str::is_empty) {
                bail!("native window script contains an empty click or assertion");
            }
            for name in clicks.split('|') {
                messages.push(format!("completed semantic click {name:?}"));
            }
            messages.push(ASSERTED.into());
            text.split('|').count() as u64
        }
        _ => bail!("native window clicks and final assertions must be declared together"),
    };
    messages.push(PRESENTED.into());
    Ok((messages, assertions))
}

pub(super) fn expected_count(env: &BTreeMap<String, String>) -> Result<u64> {
    let (messages, assertions) = script(env)?;
    // Count each click, each final text assertion, and the presented frame.
    Ok(messages.len() as u64 + assertions.saturating_sub(1))
}

fn messages(log: &str) -> Vec<&str> {
    log.lines()
        .filter_map(|line| line.strip_prefix(PREFIX))
        .filter(|line| {
            line.starts_with("completed semantic click ") || matches!(*line, ASSERTED | PRESENTED)
        })
        .collect()
}

pub(super) fn validate_streams(
    stdout: &str,
    stderr: &str,
    env: &BTreeMap<String, String>,
) -> Result<TestCounts> {
    let output = messages(stdout);
    let errors = messages(stderr);
    // Display launchers may forward application stderr to stdout. Require the
    // entire ordered script in one stream; cross-stream order is unknowable.
    if !output.is_empty() && !errors.is_empty() {
        bail!("native window certification records span both output streams");
    }
    validate(if output.is_empty() { stderr } else { stdout }, env)
}

fn validate(log: &str, env: &BTreeMap<String, String>) -> Result<TestCounts> {
    let (expected, _) = script(env)?;
    let observed = messages(log);
    if observed != expected {
        bail!("native window did not complete its exact semantic script before presenting: expected {expected:?}, observed {observed:?}");
    }
    Ok(TestCounts {
        passed: expected_count(env)?,
        failed: 0,
        ignored: 0,
        binaries: 1,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_successful_exit_cannot_replace_the_declared_native_user_journey() {
        let env = BTreeMap::from([
            ("VO_UI_AUTOMATION_EXIT_AFTER_FRAMES".into(), "1".into()),
            ("VO_UI_AUTOMATION_CLICKS".into(), "Open|Save".into()),
            ("VO_UI_AUTOMATION_EXPECT_TEXT".into(), "Ready|Saved".into()),
        ]);
        let lines = [
            "completed semantic click \"Open\"",
            "completed semantic click \"Save\"",
            ASSERTED,
            PRESENTED,
        ];
        let log = |items: &[&str]| {
            items
                .iter()
                .map(|item| format!("{PREFIX}{item}\n"))
                .collect::<String>()
        };
        assert_eq!(validate(&log(&lines), &env).unwrap().passed, 5);
        assert_eq!(validate_streams(&log(&lines), "", &env).unwrap().passed, 5);
        assert_eq!(validate_streams("", &log(&lines), &env).unwrap().passed, 5);
        assert!(validate_streams(&log(&lines[..2]), &log(&lines[2..]), &env).is_err());
        assert!(validate_streams(&log(&lines), &log(&lines), &env).is_err());
        for invalid in [
            vec![],
            vec![PRESENTED],
            vec![lines[0], ASSERTED, PRESENTED],
            vec![lines[1], lines[0], ASSERTED, PRESENTED],
            vec![lines[0], lines[1], PRESENTED, ASSERTED],
            vec![lines[0], lines[1], ASSERTED, PRESENTED, PRESENTED],
        ] {
            assert!(validate(&log(&invalid), &env).is_err(), "{invalid:?}");
        }
        let mut no_script = env.clone();
        no_script.remove("VO_UI_AUTOMATION_CLICKS");
        assert!(expected_count(&no_script).is_err());
        no_script.remove("VO_UI_AUTOMATION_EXPECT_TEXT");
        assert_eq!(validate(&log(&[PRESENTED]), &no_script).unwrap().passed, 1);
        assert!(validate(&log(&lines), &no_script).is_err());
        no_script.insert("VO_UI_AUTOMATION_EXIT_AFTER_FRAMES".into(), "0".into());
        assert!(expected_count(&no_script).is_err());
    }
}
