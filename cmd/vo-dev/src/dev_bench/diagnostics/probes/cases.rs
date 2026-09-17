//! Case arguments and returned selectors share one registry. Each requested
//! input is bound to the snapshot before a result can enter its statistics.
use super::super::Snapshot;
use super::*;
use std::path::Path;

pub(in super::super) struct Case {
    pub(in super::super) input: Option<Input>,
    pub(in super::super) args: Vec<String>,
}

impl Case {
    pub(in super::super) fn arguments(
        &self,
        probe: Probe,
        allocations: bool,
        directory: &Path,
    ) -> Vec<String> {
        let mut args = Vec::new();
        if let Some(input) = self.input {
            args.push(
                directory
                    .join(input.bytecode_file())
                    .to_string_lossy()
                    .into_owned(),
            );
        }
        args.extend(self.args.clone());
        match probe {
            Probe::Recycling | Probe::Layouts => {
                args.push(if allocations { "allocations" } else { "timing" }.into())
            }
            Probe::Island | Probe::Roots => args.push("-".into()),
            Probe::Execution => args.push(if allocations { "work" } else { "timing" }.into()),
            _ if allocations => args.push("--allocations".into()),
            _ => {}
        }
        args
    }

    pub(in super::super) fn validate(
        &self,
        artifact: &Artifact,
        data: &Value,
        args: &[String],
        snapshot: &Snapshot,
    ) -> Result<()> {
        let matches = match artifact.probe {
            Probe::Recycling => data["case"] == args[0] && data["gc_mode"] == args[1],
            Probe::Layouts => data["view"] == args[1],
            Probe::Island => {
                data["mode"] == args[1]
                    && data["children"].as_u64() == args[2].parse().ok()
                    && data["workload"] == args[3]
            }
            Probe::Execution => {
                data["mode"] == args[1]
                    && data["workload"] == args[2]
                    && data["samples"].as_u64() == args[3].parse().ok()
            }
            Probe::Roots => {
                data["mode"] == args[1]
                    && data["gc_mode"] == args[2]
                    && data["admission"] == args[3]
                    && data["source_vob"] == args[0]
            }
            _ => true,
        };
        if !matches {
            bail!("diagnostic result does not match requested case");
        }
        if let Some(input) = self.input {
            let expected = snapshot
                .inputs
                .iter()
                .find(|i| i.input == input)
                .ok_or_else(|| anyhow!("diagnostic input missing"))?;
            if if artifact.probe == Probe::Layouts {
                data["vob_bytes"] != expected.bytecode_bytes
            } else {
                data["vob_sha256"] != expected.bytecode_sha256
            } {
                bail!("diagnostic executed a different bytecode input");
            }
        }
        Ok(())
    }
}

pub(super) fn runtime_cases(probe: Probe) -> Vec<Case> {
    let mut result = Vec::new();
    for input in probe.inputs() {
        let mut add = |args: Vec<String>| {
            result.push(Case {
                input: Some(input),
                args,
            })
        };
        match probe {
            Probe::Layouts => {
                for view in ["pointers", "elements", "both"] {
                    add(vec![view.into(), LAYOUT_SAMPLES.to_string()]);
                }
            }
            Probe::Island => {
                for children in [0, 1, 8, 32, 128] {
                    for mode in ["vm", "baseline", "optimizing"] {
                        for workload in ["idle", "busy"] {
                            add(vec![
                                mode.into(),
                                children.to_string(),
                                workload.into(),
                                ISLAND_SAMPLES.to_string(),
                            ]);
                        }
                    }
                }
            }
            Probe::Roots => {
                for mode in ["vm", "jit"] {
                    for gc in ["generational", "incremental"] {
                        for admission in ["forced", "control"] {
                            add(vec![
                                mode.into(),
                                gc.into(),
                                admission.into(),
                                ROOT_SAMPLES.to_string(),
                            ]);
                        }
                    }
                }
            }
            Probe::Execution => {
                for workload in [
                    "Arithmetic",
                    "Calls",
                    "Maps",
                    "Buffered",
                    "Rendezvous",
                    "Select",
                    "Tasks",
                ] {
                    for mode in ["vm", "baseline", "optimizing", "osr"] {
                        add(vec![mode.into(), workload.into(), "32".into()]);
                    }
                }
            }
            _ => unreachable!(),
        }
    }
    result
}
