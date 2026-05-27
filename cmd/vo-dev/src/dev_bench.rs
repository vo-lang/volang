use crate::dev_common::TARGET_32;
use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

const DEFAULT_BENCH_WARMUP: u64 = 1;
const DEFAULT_BENCH_RUNS: u64 = 3;

pub(crate) fn cmd_bench(root: &Path, args: Vec<String>) -> Result<()> {
    let mut target = "all".to_string();
    let mut all_langs = false;
    let mut arch = "64".to_string();
    let mut jit_hot = false;
    let mut jit_call_threshold = None;
    let mut jit_loop_threshold = None;
    let mut warmup = DEFAULT_BENCH_WARMUP;
    let mut runs = DEFAULT_BENCH_RUNS;
    let mut target_set = false;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--all-langs" => all_langs = true,
            "--jit-hot" => jit_hot = true,
            "--arch" => {
                i += 1;
                arch = args
                    .get(i)
                    .ok_or_else(|| anyhow!("--arch requires a value"))?
                    .clone();
            }
            "--jit-call-threshold" => {
                i += 1;
                jit_call_threshold = Some(parse_u64_arg("--jit-call-threshold", args.get(i))?);
            }
            "--jit-loop-threshold" => {
                i += 1;
                jit_loop_threshold = Some(parse_u64_arg("--jit-loop-threshold", args.get(i))?);
            }
            "--warmup" => {
                i += 1;
                warmup = parse_u64_arg("--warmup", args.get(i))?;
            }
            "--runs" => {
                i += 1;
                runs = parse_positive_u64_arg("--runs", args.get(i))?;
            }
            other if other.starts_with("--jit-call-threshold=") => {
                jit_call_threshold = Some(parse_u64_value(
                    "--jit-call-threshold",
                    other.split_once('=').unwrap().1,
                )?);
            }
            other if other.starts_with("--jit-loop-threshold=") => {
                jit_loop_threshold = Some(parse_u64_value(
                    "--jit-loop-threshold",
                    other.split_once('=').unwrap().1,
                )?);
            }
            other if other.starts_with("--warmup=") => {
                warmup = parse_u64_value("--warmup", other.split_once('=').unwrap().1)?;
            }
            other if other.starts_with("--runs=") => {
                runs = parse_positive_u64_value("--runs", other.split_once('=').unwrap().1)?;
            }
            other if other.starts_with("--arch=") => {
                arch = other.split_once('=').unwrap().1.to_string();
            }
            other if other.starts_with('-') => bail!("unknown bench argument: {other}"),
            other => {
                if target_set {
                    bail!("multiple benchmark targets provided");
                }
                target = other.to_string();
                target_set = true;
            }
        }
        i += 1;
    }
    if arch != "64" && arch != "32" {
        bail!("--arch must be 32 or 64");
    }

    let runner = BenchRunner {
        root,
        target,
        all_langs,
        vo_only: false,
        arch,
        jit_hot,
        jit_call_threshold,
        jit_loop_threshold,
        warmup,
        runs,
    };
    runner.run()
}

struct BenchRunner<'a> {
    root: &'a Path,
    target: String,
    all_langs: bool,
    vo_only: bool,
    arch: String,
    jit_hot: bool,
    jit_call_threshold: Option<u64>,
    jit_loop_threshold: Option<u64>,
    warmup: u64,
    runs: u64,
}

impl BenchRunner<'_> {
    fn run(mut self) -> Result<()> {
        if self.target == "vo" {
            self.vo_only = true;
            self.target = "all".to_string();
        }
        if self.target == "score" {
            return self.calculate_scores(None, &[]);
        }
        self.check_deps()?;
        self.build_vo()?;
        let run_info = if self.target == "all" {
            self.run_all_benchmarks()?
        } else if self.benchmark_exists(&self.target)? {
            let target = self.target.clone();
            vec![self.run_benchmark(&target)?]
        } else {
            println!("Unknown benchmark: {}", self.target);
            self.list_benchmarks()?;
            bail!("unknown benchmark");
        };
        let scope: Vec<_> = run_info.iter().map(|info| info.name.clone()).collect();
        self.calculate_scores(Some(&scope), &run_info)
    }

    fn check_deps(&self) -> Result<()> {
        let mut missing = Vec::new();
        if !command_exists("hyperfine") {
            missing.push("hyperfine");
        }
        if !self.vo_only && !command_exists("go") {
            missing.push("go");
        }
        if !missing.is_empty() {
            bail!(
                "missing dependencies: {}; install with: brew install {}",
                missing.join(", "),
                missing.join(" ")
            );
        }
        Ok(())
    }

    fn build_vo(&self) -> Result<()> {
        let mut cmd = Command::new("cargo");
        cmd.args(["build", "--release", "-p", "vo"]);
        if self.arch == "32" {
            cmd.args(["--target", TARGET_32, "--no-default-features"]);
        }
        let status = cmd
            .current_dir(self.root)
            .status()
            .context("could not build vo")?;
        if !status.success() {
            bail!("cargo build --release -p vo failed");
        }
        Ok(())
    }

    fn list_benchmarks(&self) -> Result<()> {
        println!("Available benchmarks:");
        for path in benchmark_dirs(self.root)? {
            println!("  - {}", path.file_name().unwrap().to_string_lossy());
        }
        Ok(())
    }

    fn benchmark_exists(&self, name: &str) -> Result<bool> {
        Ok(benchmark_entries(self.root)?
            .iter()
            .any(|entry| entry.id == name))
    }

    fn run_all_benchmarks(&self) -> Result<Vec<BenchmarkRunInfo>> {
        let mut ran = Vec::new();
        for path in benchmark_dirs(self.root)? {
            let name = path.file_name().unwrap().to_string_lossy().to_string();
            ran.push(self.run_benchmark(&name)?);
        }
        Ok(ran)
    }

    fn run_benchmark(&self, name: &str) -> Result<BenchmarkRunInfo> {
        let bench_dir = self.root.join("benchmarks").join(name);
        let artifact_dir = self.bench_artifact_dir(name);
        fs::create_dir_all(&artifact_dir)?;
        println!("\n=== {name} ===\n");

        let vo_file = first_ext(&bench_dir, "vo")?;
        let go_file = first_ext(&bench_dir, "go")?;
        let lua_file = first_ext(&bench_dir, "lua")?;
        let js_file = first_ext(&bench_dir, "js")?;
        let py_file = first_ext(&bench_dir, "py")?;
        let rb_file = first_ext(&bench_dir, "rb")?;
        let java_file = first_ext(&bench_dir, "java")?;
        let c_file = first_ext(&bench_dir, "c")?;

        let mut commands = Vec::new();
        let mut names = Vec::new();
        let vo_bin = shell_quote(&self.vo_bench_bin());
        if let Some(vo_file) = vo_file {
            let vo_file = shell_quote(&vo_file);
            commands.push(format!("{vo_bin} run {vo_file} --mode=vm"));
            names.push("Vo-VM".to_string());
            if self.arch != "32" {
                let jit = format!(
                    "{} {vo_bin} run {vo_file} --mode=jit",
                    self.jit_env_prefix()
                )
                .trim()
                .to_string();
                names.push(if self.jit_env_prefix().is_empty() {
                    "Vo-JIT".to_string()
                } else {
                    "Vo-JIT-Hot".to_string()
                });
                commands.push(jit);
            }
        }

        if !self.vo_only {
            if let Some(go_file) = go_file {
                let go_bin = artifact_dir.join("go_bench");
                let go_cache_dir = self.bench_go_cache_dir();
                fs::create_dir_all(&go_cache_dir)?;
                if run_status(
                    self.root,
                    Command::new("go")
                        .env("GOCACHE", &go_cache_dir)
                        .args(["build", "-o"])
                        .arg(&go_bin)
                        .arg(&go_file),
                )? {
                    commands.push(shell_quote(&go_bin));
                    names.push("Go".to_string());
                }
            }
            if let Some(lua_file) = &lua_file {
                if command_exists("lua") {
                    commands.push(format!("lua {}", shell_quote(lua_file)));
                    names.push("Lua".to_string());
                }
                if command_exists("luajit") {
                    commands.push(format!("luajit {}", shell_quote(lua_file)));
                    names.push("LuaJIT".to_string());
                }
            }
            if let Some(js_file) = &js_file {
                if command_exists("node") {
                    commands.push(format!("node {}", shell_quote(js_file)));
                    names.push("Node".to_string());
                }
            }
            if let Some(py_file) = &py_file {
                if self.all_langs {
                    commands.push(format!("python3 {}", shell_quote(py_file)));
                    names.push("Python".to_string());
                }
            }
            if let Some(rb_file) = &rb_file {
                if self.all_langs && command_exists("ruby") {
                    commands.push(format!("ruby {}", shell_quote(rb_file)));
                    names.push("Ruby".to_string());
                }
            }
            if let Some(java_file) = &java_file {
                if command_exists("java") && command_exists("javac") {
                    let class_name = java_file.file_stem().unwrap().to_string_lossy().to_string();
                    if run_status(
                        self.root,
                        Command::new("javac")
                            .args(["-d"])
                            .arg(&artifact_dir)
                            .arg(java_file),
                    )? {
                        commands.push(format!(
                            "java -cp {} {class_name}",
                            shell_quote(&artifact_dir)
                        ));
                        names.push("Java".to_string());
                    }
                }
            }
        }

        if let Some(c_file) = c_file {
            let c_bin = artifact_dir.join("c_bench");
            for compiler in ["cc", "gcc", "clang"] {
                if command_exists(compiler) {
                    if run_status(
                        self.root,
                        Command::new(compiler)
                            .args(["-O3", "-o"])
                            .arg(&c_bin)
                            .arg(&c_file),
                    )? {
                        commands.push(shell_quote(&c_bin));
                        names.push("C".to_string());
                    }
                    break;
                }
            }
        }

        if commands.is_empty() {
            println!("No runnable benchmarks found");
            return Ok(BenchmarkRunInfo {
                name: name.to_string(),
                warning_count: 0,
            });
        }

        let results_dir = self.bench_results_dir();
        fs::create_dir_all(&results_dir)?;
        let export_json = results_dir.join(format!("{name}.json"));
        let export_md = results_dir.join(format!("{name}.md"));

        let mut hf = Command::new("hyperfine");
        hf.args([
            "--warmup",
            &self.warmup.to_string(),
            "--runs",
            &self.runs.to_string(),
        ]);
        for (name, command) in names.iter().zip(commands.iter()) {
            hf.args(["-n", name, command]);
        }
        hf.arg("--export-json")
            .arg(export_json)
            .arg("--export-markdown")
            .arg(export_md)
            .current_dir(self.root);
        let output = hf.output().context("could not run hyperfine")?;
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        print!("{stdout}");
        eprint!("{stderr}");
        let warning_count =
            count_hyperfine_warnings(&output.stdout) + count_hyperfine_warnings(&output.stderr);
        if !output.status.success() {
            bail!("hyperfine failed for {name}");
        }
        Ok(BenchmarkRunInfo {
            name: name.to_string(),
            warning_count,
        })
    }

    fn calculate_scores(
        &self,
        only: Option<&[String]>,
        run_info: &[BenchmarkRunInfo],
    ) -> Result<()> {
        println!("\n=== Calculating Scores ===\n");
        let results_dir = self.bench_results_dir();
        let selected: Option<BTreeSet<_>> =
            only.map(|items| items.iter().map(String::as_str).collect());
        let mut files = Vec::new();
        if results_dir.is_dir() {
            for entry in fs::read_dir(&results_dir)? {
                let path = entry?.path();
                if path.extension().and_then(|ext| ext.to_str()) != Some("json") {
                    continue;
                }
                let stem = path.file_stem().unwrap().to_string_lossy();
                if stem == "summary" {
                    continue;
                }
                if selected
                    .as_ref()
                    .is_none_or(|items| items.contains(stem.as_ref()))
                {
                    files.push(path);
                }
            }
        }
        files.sort();
        if files.is_empty() {
            println!("No results found. Run benchmarks first.");
            return Ok(());
        }

        let mut scores: BTreeMap<String, Vec<f64>> = BTreeMap::new();
        for file in &files {
            let benchmark_name = file.file_stem().unwrap().to_string_lossy();
            println!("Processing: {benchmark_name}");
            let text = fs::read_to_string(file)?;
            if text.trim().is_empty() {
                println!("  Skipping: empty file");
                continue;
            }
            let data: Value = match serde_json::from_str(&text) {
                Ok(data) => data,
                Err(err) => {
                    println!("  Skipping: invalid JSON ({err})");
                    continue;
                }
            };
            let Some(results) = data.get("results").and_then(Value::as_array) else {
                continue;
            };
            let mut means = BTreeMap::new();
            for result in results {
                let Some(name) = result.get("command").and_then(Value::as_str) else {
                    continue;
                };
                if !matches!(
                    name,
                    "Vo-VM"
                        | "Vo-JIT"
                        | "Vo-JIT-Hot"
                        | "Go"
                        | "Lua"
                        | "LuaJIT"
                        | "Node"
                        | "Python"
                        | "Ruby"
                        | "Java"
                        | "C"
                ) {
                    continue;
                }
                if let Some(mean) = result.get("mean").and_then(Value::as_f64) {
                    if mean > 0.0 {
                        means.insert(name.to_string(), mean);
                    }
                }
            }
            if means.is_empty() {
                continue;
            }
            let baseline = if self.vo_only {
                means
                    .get("Vo-VM")
                    .copied()
                    .unwrap_or_else(|| means.values().copied().fold(f64::INFINITY, f64::min))
            } else {
                means.values().copied().fold(f64::INFINITY, f64::min)
            };
            for (name, mean) in means {
                let score = if self.vo_only {
                    (mean / baseline) * 100.0
                } else {
                    mean / baseline
                };
                scores.entry(name.clone()).or_default().push(score);
                if self.vo_only {
                    println!("  {name}: {score:.1} (mean: {mean:.4}s)");
                } else {
                    println!("  {name}: {score:.2}x (mean: {mean:.4}s)");
                }
            }
        }

        if scores.is_empty() {
            println!("\nNo valid results to analyze.");
            return Ok(());
        }
        let mut averages: Vec<_> = scores
            .iter()
            .map(|(name, values)| {
                let avg = values.iter().sum::<f64>() / values.len() as f64;
                (name.clone(), avg)
            })
            .collect();
        averages.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));

        if self.vo_only {
            println!("\nLanguage Performance (Vo-VM = 100, lower is faster):");
            for (idx, (name, score)) in averages.iter().enumerate() {
                let marker = if name == "Vo-VM" { " <- baseline" } else { "" };
                println!("{:>2}. {:<10}: {:>7.1}{marker}", idx + 1, name, score);
            }
        } else {
            println!("\nLanguage Performance Ranking (lower relative time is better):");
            for (idx, (name, score)) in averages.iter().enumerate() {
                println!("{:>2}. {:<10}: {:.2}x", idx + 1, name, score);
            }
        }
        if !run_info.is_empty() {
            self.write_summary(only, run_info, &averages)?;
        }
        Ok(())
    }

    fn write_summary(
        &self,
        only: Option<&[String]>,
        run_info: &[BenchmarkRunInfo],
        ranking: &[(String, f64)],
    ) -> Result<()> {
        let results_dir = self.bench_results_dir();
        fs::create_dir_all(&results_dir)?;
        let summary = BenchmarkSummary {
            schema: "volang.benchmark.summary.v1",
            generated_at_unix_sec: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            scope: only.map(|items| items.to_vec()).unwrap_or_default(),
            config: BenchmarkSummaryConfig {
                runs: self.runs,
                warmup: self.warmup,
                all_langs: self.all_langs,
                vo_only: self.vo_only,
                arch: self.arch.clone(),
                jit_hot: self.jit_hot,
                jit_call_threshold: self.jit_call_threshold,
                jit_loop_threshold: self.jit_loop_threshold,
                results_dir: path_display(self.root, &results_dir),
                artifacts_dir: path_display(self.root, &self.bench_artifacts_dir()),
                go_cache_dir: path_display(self.root, &self.bench_go_cache_dir()),
                vo_binary: path_display(self.root, &self.vo_bench_bin()),
                score_mode: if self.vo_only {
                    "vo_vm_100".to_string()
                } else {
                    "relative_time".to_string()
                },
            },
            tools: collect_tool_versions(),
            runs: run_info.to_vec(),
            ranking: ranking
                .iter()
                .enumerate()
                .map(|(idx, (name, score))| BenchmarkRankingEntry {
                    rank: idx + 1,
                    name: name.clone(),
                    score: *score,
                })
                .collect(),
        };
        let path = results_dir.join("summary.json");
        fs::write(&path, serde_json::to_string_pretty(&summary)?)?;
        println!(
            "\nWrote benchmark summary: {}",
            path_display(self.root, &path)
        );
        Ok(())
    }

    fn jit_env_prefix(&self) -> String {
        let mut parts = Vec::new();
        if let Some(value) = self.jit_call_threshold {
            parts.push(format!("VO_JIT_CALL_THRESHOLD={value}"));
        } else if self.jit_hot {
            parts.push("VO_JIT_CALL_THRESHOLD=1".to_string());
        }
        if let Some(value) = self.jit_loop_threshold {
            parts.push(format!("VO_JIT_LOOP_THRESHOLD={value}"));
        } else if self.jit_hot {
            parts.push("VO_JIT_LOOP_THRESHOLD=999999".to_string());
        }
        parts.join(" ")
    }

    fn vo_bench_bin(&self) -> PathBuf {
        if self.arch == "32" {
            return self.root.join("target").join(TARGET_32).join("release/vo");
        }
        self.root.join("target/release/vo")
    }

    fn bench_results_dir(&self) -> PathBuf {
        self.root.join("target/bench/results")
    }

    fn bench_artifacts_dir(&self) -> PathBuf {
        self.root.join("target/bench/artifacts")
    }

    fn bench_artifact_dir(&self, name: &str) -> PathBuf {
        self.bench_artifacts_dir().join(name)
    }

    fn bench_go_cache_dir(&self) -> PathBuf {
        self.root.join("target/bench/go-cache")
    }
}

#[derive(Debug, Clone, Serialize)]
struct BenchmarkRunInfo {
    name: String,
    warning_count: usize,
}

#[derive(Debug, Serialize)]
struct BenchmarkSummary {
    schema: &'static str,
    generated_at_unix_sec: u64,
    scope: Vec<String>,
    config: BenchmarkSummaryConfig,
    tools: Vec<ToolVersion>,
    runs: Vec<BenchmarkRunInfo>,
    ranking: Vec<BenchmarkRankingEntry>,
}

#[derive(Debug, Serialize)]
struct BenchmarkSummaryConfig {
    runs: u64,
    warmup: u64,
    all_langs: bool,
    vo_only: bool,
    arch: String,
    jit_hot: bool,
    jit_call_threshold: Option<u64>,
    jit_loop_threshold: Option<u64>,
    results_dir: String,
    artifacts_dir: String,
    go_cache_dir: String,
    vo_binary: String,
    score_mode: String,
}

#[derive(Debug, Serialize)]
struct ToolVersion {
    name: String,
    version: Option<String>,
}

#[derive(Debug, Serialize)]
struct BenchmarkRankingEntry {
    rank: usize,
    name: String,
    score: f64,
}

#[derive(Debug, Deserialize)]
struct BenchmarkManifest {
    version: u32,
    #[serde(default, rename = "benchmark")]
    benchmarks: Vec<BenchmarkEntry>,
}

#[derive(Debug, Deserialize)]
struct BenchmarkEntry {
    id: String,
    path: String,
    owner: String,
    #[serde(default)]
    languages: Vec<String>,
}

fn benchmark_entries(root: &Path) -> Result<Vec<BenchmarkEntry>> {
    let path = root.join("benchmarks/manifest.toml");
    let text =
        fs::read_to_string(&path).with_context(|| format!("could not read {}", path.display()))?;
    let manifest: BenchmarkManifest =
        toml::from_str(&text).with_context(|| format!("could not parse {}", path.display()))?;
    if manifest.version != 1 {
        bail!("benchmarks/manifest.toml version must be 1");
    }
    Ok(manifest.benchmarks)
}

fn benchmark_dirs(root: &Path) -> Result<Vec<PathBuf>> {
    let mut dirs = Vec::new();
    for entry in benchmark_entries(root)? {
        if entry.id.trim().is_empty()
            || entry.path.trim().is_empty()
            || entry.owner.trim().is_empty()
        {
            bail!("benchmarks/manifest.toml contains an incomplete benchmark entry");
        }
        if entry.languages.is_empty() {
            bail!("benchmark {} must declare languages", entry.id);
        }
        let path = root.join("benchmarks").join(&entry.path);
        if !path.is_dir() {
            bail!("benchmark {} path is missing: {}", entry.id, entry.path);
        }
        dirs.push(path);
    }
    Ok(dirs)
}

fn first_ext(dir: &Path, ext: &str) -> Result<Option<PathBuf>> {
    for entry in fs::read_dir(dir).with_context(|| format!("could not read {}", dir.display()))? {
        let path = entry?.path();
        if path.extension().and_then(|value| value.to_str()) == Some(ext) {
            return Ok(Some(path));
        }
    }
    Ok(None)
}

fn command_exists(cmd: &str) -> bool {
    Command::new("sh")
        .args(["-c", &format!("command -v {}", shell_quote_str(cmd))])
        .output()
        .is_ok_and(|output| output.status.success())
}

fn run_status(root: &Path, command: &mut Command) -> Result<bool> {
    Ok(command.current_dir(root).status()?.success())
}

fn parse_positive_u64_arg(name: &str, value: Option<&String>) -> Result<u64> {
    parse_positive_u64_value(
        name,
        value.ok_or_else(|| anyhow!("{name} requires a value"))?,
    )
}

fn parse_positive_u64_value(name: &str, value: &str) -> Result<u64> {
    let parsed = parse_u64_value(name, value)?;
    if parsed == 0 {
        bail!("{name} must be > 0");
    }
    Ok(parsed)
}

fn parse_u64_arg(name: &str, value: Option<&String>) -> Result<u64> {
    parse_u64_value(
        name,
        value.ok_or_else(|| anyhow!("{name} requires a value"))?,
    )
}

fn parse_u64_value(name: &str, value: &str) -> Result<u64> {
    value
        .parse::<u64>()
        .with_context(|| format!("{name} must be an integer"))
}

fn shell_quote(path: &Path) -> String {
    shell_quote_str(&path.to_string_lossy())
}

fn shell_quote_str(value: &str) -> String {
    if value
        .chars()
        .all(|ch| ch.is_ascii_alphanumeric() || "-_./:=+".contains(ch))
    {
        value.to_string()
    } else {
        format!("'{}'", value.replace('\'', "'\"'\"'"))
    }
}

fn count_hyperfine_warnings(output: &[u8]) -> usize {
    String::from_utf8_lossy(output).matches("Warning:").count()
}

fn collect_tool_versions() -> Vec<ToolVersion> {
    [
        "hyperfine",
        "rustc",
        "cargo",
        "go",
        "lua",
        "luajit",
        "node",
        "python3",
        "ruby",
        "java",
        "javac",
        "cc",
    ]
    .into_iter()
    .map(|name| ToolVersion {
        name: name.to_string(),
        version: command_version(name),
    })
    .collect()
}

fn command_version(name: &str) -> Option<String> {
    let args: &[&str] = match name {
        "go" => &["version"],
        "lua" | "luajit" => &["-v"],
        "java" | "javac" => &["-version"],
        _ => &["--version"],
    };
    let output = Command::new(name).args(args).output().ok()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let text = if stdout.trim().is_empty() {
        stderr.trim()
    } else {
        stdout.trim()
    };
    if text.is_empty() {
        None
    } else {
        text.lines().next().map(str::to_string)
    }
}

fn path_display(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .to_string()
}
