# Continuous Integration

Volang uses four GitHub Actions workflows. The workflow files are the
executable source of truth; this document explains their stable responsibilities
and the commands maintainers can reproduce locally.

All third-party actions are pinned to full commit SHAs. CI and Nightly have
read-only repository permissions. Write permissions exist only in the deploy
job for Pages and the protected publish job for releases.

## Workflows

### [`ci.yml`](../.github/workflows/ci.yml)

Runs for pull requests, merge groups, pushes to `main`, and manual dispatches.
There are no path filters or changed-file planners: every run uses the same
small event-defined job graph.

| Lane | Responsibility |
| --- | --- |
| `quality-rust` | Rust formatting, repository and language-manifest lint, Action workflow lint, Clippy, root-workspace tests, and a clean generated state |
| `language-native` | Pull requests run one `smoke` selection on VM, JIT, and compile targets; merge groups, `main`, and manual runs split the complete native VM/JIT/OSR/GC/no_std/compile selection into two stable case shards |
| `wasm-web` | WASM language cases, `vo-web`, Studio Web, and standalone Studio WASM |
| `studio-native` | Standalone Tauri Clippy and tests on macOS |
| `required` | Fails unless every lane above succeeds; configure this stable job name as the required branch-protection check |

Pull-request and superseded merge-queue runs cancel older work for the same
ref. Every `main` push uses its immutable SHA as the concurrency key, so no
successful-CI candidate can be displaced by a later push.

### [`nightly.yml`](../.github/workflows/nightly.yml)

Runs daily at 03:37 UTC and on manual dispatch. It adds work that is valuable
but too expensive or platform-specific for every pull request:

- complete native and WASM language matrices in release mode;
- repeated GC, JIT/OSR, and scheduler stress selections;
- root-workspace tests on macOS and Windows, plus the standalone Tauri
  workspace on Windows;
- Rust audits for all maintained lockfiles and npm audits for Studio and
  `vo-web`.

Nightly never publishes or mutates repository contents. A nightly failure is a
maintenance signal and does not replace the required pull-request check.

### [`site.yml`](../.github/workflows/site.yml)

Runs after a successful `CI` push run whose exact SHA is still the current
`main` head, or by manual dispatch from `main`. The freshness condition prevents
a slower, older CI run from rolling Pages back. It builds Studio WASM and the
web application, checks the frontend, tests guest-exit handling, uploads
`apps/studio/dist`, and deploys that artifact to GitHub Pages.

The build job has read-only permissions. Only the deploy job receives
`pages: write` and `id-token: write`. Configure the repository's Pages source
to GitHub Actions before the first deployment.

### [`release.yml`](../.github/workflows/release.yml)

Runs only for a `repository_dispatch` event of type `release`, carrying an
existing `v*` tag, and places requests in one bounded serial queue. GitHub
binds repository-dispatch workflows to the default branch, so a tag cannot
supply publication logic. Its phases are intentionally separate:

1. `preflight` requires a successful `ci.yml` push run for the exact tagged
   commit, binds the tag and checkout to that immutable SHA, verifies reachability
   from `origin/main`, validates release policy, and derives the target matrix
   and deterministic identity.
2. `build` compiles and smoke-tests each target declared in
   `eng/release.toml`, then uploads its archive, checksum, and provenance.
3. `publish` downloads and verifies the complete artifact set, creates GitHub
   build-provenance attestations, and publishes one GitHub Release.

SDK publication and Homebrew repository updates are separate operations and
are not performed by this workflow.

## Local checks

Use Rust 1.94.0, Node 24, and wasm-pack 0.14.0, matching
`rust-toolchain.toml` and `eng/toolchains.toml`. Run commands from the repository
root unless a command names another manifest. Match the workflow's workspace
boundary before running repository or language commands:

```sh
export VOWORK=off
```

### Quality and root workspace

```sh
cargo fmt --all -- --check
cargo fmt --manifest-path apps/studio/wasm/Cargo.toml -- --check
cargo fmt --manifest-path apps/studio/src-tauri/Cargo.toml -- --check
cargo run -q -p vo-dev --locked -- lint all
cargo run -q -p vo-dev --locked -- test lint --suite lang --strict
cargo run -q -p vo-dev --locked -- test fmt --suite lang
cargo clippy --workspace --all-targets --locked -- -D warnings
cargo test --workspace --all-targets --locked
```

CI also runs actionlint 1.7.12. Its schema predates GitHub's `concurrency.queue`
key, so the local equivalent keeps one narrow compatibility exception:

```sh
actionlint -ignore 'unexpected key "queue"'
```

### Language targets

Pull-request selection:

```sh
cargo run -q -p vo-dev --locked -- test run --suite lang --tags smoke --targets both,compile
```

Complete native and WASM selections:

```sh
cargo run -q -p vo-dev --locked -- test run --suite lang --targets native,gc,embed,compile --shard 1/2
cargo run -q -p vo-dev --locked -- test run --suite lang --targets native,gc,embed,compile --shard 2/2
cargo run -q -p vo-dev --locked -- test run --suite lang --targets wasm
```

Sharding hashes the case ID before target expansion. Every target variant of a
case stays in one shard, preserving VM/JIT/OSR and GC differential checks while
the two shards run concurrently.

The WASM lane also checks the portable dependency closure and compiles each
supported no-default-feature configuration:

```sh
cargo tree --locked -p vo-stdlib --target wasm32-unknown-unknown --no-default-features --edges normal,no-proc-macro --format '{p}|{f}'
cargo tree --locked -p vo-vm --target wasm32-unknown-unknown --no-default-features --edges normal,no-proc-macro --format '{p}|{f}'
cargo tree --locked -p vo-ext --target wasm32-unknown-unknown --no-default-features --features wasm --edges normal,no-proc-macro --format '{p}|{f}'
cargo check --locked -p vo-stdlib -p vo-vm --no-default-features
cargo check --locked -p vo-vm --target wasm32-unknown-unknown --no-default-features
cargo check --locked -p vo-ext --target wasm32-unknown-unknown --no-default-features --features wasm --tests
```

Add `--release` to reproduce the nightly release-mode runs. The three nightly
stress selections are:

```sh
cargo run -q -p vo-dev --locked -- test run --suite lang --targets gc-vm,gc-jit --tags gc --repeat 20 --release
cargo run -q -p vo-dev --locked -- test run --suite lang --targets jit,osr --tags jit --repeat 5 --release
cargo run -q -p vo-dev --locked -- test run --suite lang --targets vm,jit --tags scheduler --repeat 10 --release
```

### Web and Studio

```sh
npm --prefix lang/crates/vo-web ci
npm --prefix lang/crates/vo-web run test:vfs

npm --prefix apps/studio ci
npm --prefix apps/studio run build:wasm
npm --prefix apps/studio run check
npm --prefix apps/studio run test:guest-exit
npm --prefix apps/studio run build
wasm-pack test --node --release apps/studio/wasm --locked

cargo clippy --manifest-path apps/studio/src-tauri/Cargo.toml --all-targets --locked -- -D warnings
cargo test --manifest-path apps/studio/src-tauri/Cargo.toml --locked
```

The Studio commands above also reproduce the Pages build; the deployable output
is `apps/studio/dist`.

### Dependency audits

```sh
cargo audit --file Cargo.lock
cargo audit --file apps/studio/wasm/Cargo.lock
cargo audit --file apps/studio/src-tauri/Cargo.lock
npm --prefix apps/studio audit --audit-level high
npm --prefix lang/crates/vo-web audit --audit-level high
```

## Release environment

Before publishing the first release:

1. Create a GitHub environment named `release`.
2. Add the desired required reviewers so publication has an explicit approval
   boundary.
3. Prevent self-review, disable administrator bypass, and restrict that
   environment to `main`.
4. Add an environment secret named `RELEASE_SETTINGS_TOKEN`, backed by a
   fine-grained token scoped only to this repository with `Administration:
   read`. The release job uses it solely to fail closed when immutable releases
   cannot be confirmed before publication.
5. Keep `required` green on the exact `main` commit that will receive the tag.
6. Ensure the tag version agrees with the workspace version and that the tag
   points to a commit reachable from `origin/main`.
7. Enable immutable releases and protect `v*` tags from updates and deletion.

Safe local release preflight commands are:

```sh
cargo build --locked -p vo-dev
./target/debug/vo-dev lint release
./target/debug/vo-dev release matrix
./target/debug/vo-dev release metadata --tag vX.Y.Z --commit <full-commit-sha>
```

Release build, package, verify, and publish commands require the clean tagged
source identity used by the workflow. Do not invoke `release publish` for local
validation. Push the approved tag, then request the trusted default-branch
workflow with:

```sh
gh api --method POST repos/vo-lang/volang/dispatches \
  -f event_type=release \
  -f 'client_payload[tag]=vX.Y.Z'
```
