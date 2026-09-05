"""Bounded diagnostic for the native extension's virtual Cargo workspace."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile


def changed_paths(left, right, path=""):
    if type(left) is not type(right):
        return [path]
    if isinstance(left, dict):
        return [item for key in sorted(left.keys() | right.keys())
                for item in changed_paths(left.get(key), right.get(key), f"{path}/{key}")]
    if isinstance(left, list):
        if len(left) != len(right):
            return [path + "/length"]
        return [item for i, pair in enumerate(zip(left, right))
                for item in changed_paths(*pair, f"{path}/{i}")]
    return [] if left == right else [path]


def main():
    output = Path("target/ci/native-metadata-probe").resolve()
    output.mkdir(parents=True, exist_ok=True)
    environment = dict(os.environ, VOWORK="off")
    environment.pop("CARGO_BUILD_TARGET", None)
    target = "x86_64-pc-windows-msvc" if os.name == "nt" else "x86_64-unknown-linux-gnu"
    with tempfile.TemporaryDirectory(prefix="vo_native_virtual_cargo_workspace_member_") as temporary:
        root = Path(temporary).resolve() / "module" / "rust"
        files = {
            "Cargo.toml": '[workspace]\nresolver = "2"\nmembers = ["ext", "protocol"]\n',
            ".cargo/config.toml": '[build]\ntarget-dir = "build-output"\n',
            "ext/Cargo.toml": '[package]\nname="demo"\nversion="0.1.0"\nedition="2021"\n[lib]\ncrate-type=["cdylib"]\n[dependencies]\nprotocol={path="../protocol"}\n',
            "ext/src/lib.rs": 'pub fn value() -> u8 { protocol::VERSION }\n',
            "protocol/Cargo.toml": '[package]\nname="protocol"\nversion="0.1.0"\nedition="2021"\n',
            "protocol/src/lib.rs": 'pub const VERSION: u8 = 1;\n',
        }
        for relative, content in files.items():
            path = root / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(content, encoding="utf-8")
        # Rust's Windows canonicalize returns a verbatim path. Preserve that
        # invocation shape, including the working directory and manifest.
        cwd = str(root)
        if os.name == "nt":
            cwd = "\\\\?\\" + cwd
        manifest = cwd + os.sep + "Cargo.toml"
        subprocess.run(["cargo", "generate-lockfile", "--manifest-path", manifest],
                       cwd=cwd, env=environment, check=True, timeout=60)
        command = ["cargo", "metadata", "--format-version", "1", "--locked",
                   "--filter-platform", target, "--manifest-path", manifest]
        observations = []
        previous_raw = None
        previous = None
        for index in range(32):
            if index == 8:
                (root / "protocol/src/lib.rs").write_text('pub const VERSION: u8 = 2;\n', encoding="utf-8")
            child = subprocess.run(command, cwd=cwd, env=environment, check=True,
                                   capture_output=True, timeout=60)
            current = json.loads(child.stdout)
            changes = changed_paths(previous, current) if previous is not None else []
            raw_changed = previous_raw is not None and previous_raw != child.stdout
            observations.append({"index": index, "sha256": hashlib.sha256(child.stdout).hexdigest(),
                                 "raw_changed": raw_changed, "changed_fields": changes})
            if raw_changed:
                (output / f"{index}-before.json").write_bytes(previous_raw)
                (output / f"{index}-after.json").write_bytes(child.stdout)
                print(f"metadata read {index}: {changes}", flush=True)
            previous_raw, previous = child.stdout, current
        (output / "result.json").write_text(json.dumps({"target": target, "observations": observations}, indent=2) + "\n")
        changed = sum(item["raw_changed"] for item in observations)
        print(f"Observed {changed} metadata changes across 32 reads", flush=True)
        return int(changed != 0)


if __name__ == "__main__":
    raise SystemExit(main())
