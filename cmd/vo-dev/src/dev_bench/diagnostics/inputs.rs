//! Frozen bytecode inputs for runtime-only diagnostics. Compilation belongs to
//! preparation; measurements consume authenticated, equal-length input paths.
use super::*;
use std::fmt::Write as _;

#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(super) enum Input {
    LayoutNumeric,
    LayoutPointers,
    LayoutAggregates,
    IslandPoll,
    Execution,
    Globals0,
    Globals256,
    Globals8192,
    Fibers16,
    Fibers1024,
    Defers16,
    Defers512,
}

impl Input {
    pub(super) fn name(self) -> &'static str {
        match self {
            Self::LayoutNumeric => "layout-numeric",
            Self::LayoutPointers => "layout-pointers",
            Self::LayoutAggregates => "layout-aggregates",
            Self::IslandPoll => "island-poll",
            Self::Execution => "execution",
            Self::Globals0 => "globals-0",
            Self::Globals256 => "globals-256",
            Self::Globals8192 => "globals-8192",
            Self::Fibers16 => "fibers-16",
            Self::Fibers1024 => "fibers-1024",
            Self::Defers16 => "defers-16",
            Self::Defers512 => "defers-512",
        }
    }

    pub(super) fn source(self, root: &Path) -> Result<String> {
        let path = match self {
            Self::LayoutNumeric => "benchmarks/sum-array/sum.vo",
            Self::LayoutPointers => "benchmarks/binary-trees/trees.vo",
            Self::LayoutAggregates => "benchmarks/codegen-storage/codegen_storage.vo",
            Self::IslandPoll => "lang/crates/vo-engine/examples/island_poll.vo",
            Self::Execution => "lang/crates/vo-engine/examples/execution_phases.vo",
            _ => return Ok(self.root_source()),
        };
        fs::read_to_string(root.join(path)).with_context(|| format!("diagnostic fixture {path}"))
    }

    fn root_source(self) -> String {
        let mut source = String::from(
            "package main\ntype Node struct { value int }\nvar gate = make(chan bool)\n\
             func keep(node *Node, want int) { assert(node.value == want, \"live root lost across GC\") }\n\
             func observe() { println(\"ROOTS_READY\"); <-gate }\n\
             func LatencyProbe() { println(\"ROOTS_PROBE\") }\n\
             func ReleaseRoots() { close(gate) }\n",
        );
        match self {
            Self::Globals0 | Self::Globals256 | Self::Globals8192 => {
                let count = match self {
                    Self::Globals0 => 0,
                    Self::Globals256 => 256,
                    _ => 8192,
                };
                for i in 0..count {
                    writeln!(source, "var g{i} *Node").unwrap();
                }
                source.push_str("func main() {\n");
                for i in 0..count {
                    writeln!(source, "g{i} = &Node{{{i}}}").unwrap();
                }
                source.push_str("observe()\n");
                for i in 0..count {
                    writeln!(source, "keep(g{i}, {i})").unwrap();
                }
                source.push_str("println(\"ROOTS_OK\")\n}\n");
            }
            Self::Fibers16 | Self::Fibers1024 => {
                let count = if self == Self::Fibers16 { 16 } else { 1024 };
                writeln!(source, "func worker(id int, ready chan int, resume chan bool, done chan int) {{
                    node := &Node{{id}}; ready <- 1; <-resume; keep(node, id); done <- id
                }}
                func main() {{
                    ready := make(chan int, {count}); resume := make(chan bool); done := make(chan int, {count})
                    for i := 0; i < {count}; i++ {{ go worker(i, ready, resume, done) }}
                    for i := 0; i < {count}; i++ {{ <-ready }}
                    observe(); close(resume); total := 0
                    for i := 0; i < {count}; i++ {{ total += <-done }}
                    assert(total == {}, \"all Fiber roots survived\"); println(\"ROOTS_OK\")
                }}", count * (count - 1) / 2).unwrap();
            }
            Self::Defers16 | Self::Defers512 => {
                let depth = if self == Self::Defers16 { 16 } else { 512 };
                writeln!(
                    source,
                    "func descend(depth int) {{
                    node := &Node{{depth}}; defer keep(node, depth)
                    if depth == 0 {{ observe(); return }}
                    descend(depth - 1); keep(node, depth)
                }}
                func main() {{ descend({depth}); println(\"ROOTS_OK\") }}"
                )
                .unwrap();
            }
            _ => unreachable!("non-root inputs have maintained source files"),
        }
        source
    }

    pub(super) fn source_file(self) -> String {
        format!("inputs/{}/main.vo", self.name())
    }
    pub(super) fn bytecode_file(self) -> String {
        format!("inputs/{}/program.vob", self.name())
    }
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct FrozenInput {
    pub(super) input: Input,
    pub(super) source_sha256: String,
    pub(super) bytecode_sha256: String,
    pub(super) bytecode_bytes: u64,
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Compiler {
    pub(super) sha256: String,
}

pub(super) fn compiler_file() -> String {
    format!("input-compiler/vo{}", std::env::consts::EXE_SUFFIX)
}

pub(super) fn prepare(
    root: &Path,
    output: &Path,
    probes: &[Probe],
    cancelled: &AtomicBool,
) -> Result<(Vec<FrozenInput>, Option<Compiler>)> {
    let inputs = probes
        .iter()
        .flat_map(|p| p.inputs())
        .collect::<BTreeSet<_>>();
    if inputs.is_empty() {
        return Ok((Vec::new(), None));
    }
    let directory = output.join("input-compiler");
    fs::create_dir(&directory)?;
    let mut command = clean_command("cargo");
    command
        .current_dir(root)
        .args([
            "build",
            "--locked",
            "--profile",
            "release-native",
            "-p",
            "vo",
        ])
        .env_remove("CARGO_BUILD_TARGET")
        .env_remove("CARGO_TARGET_DIR");
    execute(command, &directory, cancelled, Duration::from_secs(1800))?;
    let compiler = output.join(compiler_file());
    fs::copy(
        root.join(format!(
            "target/release-native/vo{}",
            std::env::consts::EXE_SUFFIX
        )),
        &compiler,
    )?;
    let identity = Compiler {
        sha256: sha256_file(&compiler)?,
    };
    let mut frozen = Vec::new();
    for input in inputs {
        let source = output.join(input.source_file());
        fs::create_dir_all(source.parent().unwrap())?;
        fs::write(&source, input.source(root)?)?;
        let source_sha256 = sha256_file(&source)?;
        // Debug paths are part of VOB identity. A content-addressed preparation
        // path gives equal fixtures the same source name across snapshots.
        let compiler_source = root
            .join("target/bench/diagnostic-inputs")
            .join(format!("{}-{source_sha256}", input.name()))
            .join("main.vo");
        fs::create_dir_all(compiler_source.parent().unwrap())?;
        if !compiler_source.exists() {
            fs::copy(&source, &compiler_source)?;
        }
        if sha256_file(&compiler_source)? != source_sha256 {
            bail!("diagnostic fixture staging changed");
        }
        let bytecode = output.join(input.bytecode_file());
        let mut command = clean_command(&compiler);
        command
            .current_dir(root)
            .args(["build", "--kind=bytecode", "--no-cache", "-o"])
            .arg(&bytecode)
            .arg(&compiler_source);
        execute(
            command,
            source.parent().unwrap(),
            cancelled,
            Duration::from_secs(300),
        )?;
        if sha256_file(&compiler_source)? != source_sha256 {
            bail!("diagnostic fixture changed during compilation");
        }
        frozen.push(FrozenInput {
            input,
            source_sha256,
            bytecode_sha256: sha256_file(&bytecode)?,
            bytecode_bytes: fs::metadata(&bytecode)?.len(),
        });
    }
    if sha256_file(&compiler)? != identity.sha256 {
        bail!("diagnostic input compiler changed");
    }
    Ok((frozen, Some(identity)))
}

pub(super) fn validate(snapshot: &Snapshot, directory: &Path) -> Result<()> {
    let expected = snapshot
        .artifacts
        .iter()
        .flat_map(|a| a.probe.inputs())
        .collect::<BTreeSet<_>>();
    let actual = snapshot
        .inputs
        .iter()
        .map(|i| i.input)
        .collect::<BTreeSet<_>>();
    if expected != actual
        || actual.len() != snapshot.inputs.len()
        || snapshot.input_compiler.is_some() != !actual.is_empty()
    {
        bail!("incomplete or duplicate diagnostic input set");
    }
    if let Some(compiler) = &snapshot.input_compiler {
        verify_file(directory, &compiler_file(), &compiler.sha256)?;
    }
    for input in &snapshot.inputs {
        verify_file(directory, &input.input.source_file(), &input.source_sha256)?;
        verify_file(
            directory,
            &input.input.bytecode_file(),
            &input.bytecode_sha256,
        )?;
        if fs::metadata(directory.join(input.input.bytecode_file()))?.len() != input.bytecode_bytes
        {
            bail!("diagnostic input length mismatch");
        }
    }
    Ok(())
}

fn verify_file(directory: &Path, relative: &str, digest: &str) -> Result<()> {
    let path = directory.join(relative).canonicalize()?;
    if !path.starts_with(directory) || sha256_file(&path)? != digest {
        bail!("diagnostic input identity mismatch: {relative}");
    }
    Ok(())
}

pub(super) fn matching(a: &Snapshot, b: &Snapshot) -> Result<()> {
    let records = |s: &Snapshot| {
        s.inputs
            .iter()
            .map(|i| {
                (
                    i.input,
                    (i.source_sha256.clone(), i.bytecode_sha256.clone()),
                )
            })
            .collect::<BTreeMap<_, _>>()
    };
    if records(a) != records(b) {
        bail!("runtime diagnostic comparisons require identical source and bytecode inputs");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn root_fixtures_keep_separate_roots_and_validate_them_after_release() {
        let globals = Input::Globals8192.root_source();
        assert_eq!(
            globals
                .lines()
                .filter(|s| s.starts_with("var g") && s.ends_with("*Node"))
                .count(),
            8192
        );
        assert_eq!(
            globals.lines().filter(|s| s.starts_with("keep(g")).count(),
            8192
        );
        assert!(globals.find("g8191 = &Node{8191}").unwrap() < globals.rfind("observe()").unwrap());
        assert!(globals.rfind("observe()").unwrap() < globals.find("keep(g0, 0)").unwrap());
        assert!(Input::Fibers1024.root_source().contains("total == 523776"));
        assert!(Input::Defers512.root_source().contains("descend(512)"));
    }
}
