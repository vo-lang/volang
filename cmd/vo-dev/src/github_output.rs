use anyhow::{Context, Result};
use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

pub(crate) fn write_github_output(values: &[(&str, String)]) -> Result<()> {
    let Some(path) = env::var_os("GITHUB_OUTPUT") else {
        for (key, value) in values {
            println!("{key}={value}");
        }
        return Ok(());
    };
    let mut out = String::new();
    for (key, value) in values {
        if value.contains('\n') {
            let delimiter = github_output_delimiter(key, value);
            out.push_str(key);
            out.push_str("<<");
            out.push_str(&delimiter);
            out.push('\n');
            out.push_str(value);
            out.push('\n');
            out.push_str(&delimiter);
            out.push('\n');
        } else {
            out.push_str(key);
            out.push('=');
            out.push_str(value);
            out.push('\n');
        }
    }
    let mut file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(PathBuf::from(path))
        .context("could not open GITHUB_OUTPUT")?;
    file.write_all(out.as_bytes())
        .context("could not write GITHUB_OUTPUT")
}

fn github_output_delimiter(key: &str, value: &str) -> String {
    let mut delimiter = format!("VO_DEV_{}_EOF", key.to_ascii_uppercase());
    while value.contains(&delimiter) {
        delimiter.push('_');
    }
    delimiter
}
