# Web and desktop products

E5 turns the shared Volang UI component graph into deployable Web and desktop
products. Application logic remains typed Volang source. Development can use
the VM or JIT; release artifacts use Core Wasm AOT or Native AOT.

## Web product

`ui/web` owns route, loader, action, cache, metadata, asset, activation, origin,
CSRF and deployment contracts. `ui/web/server` owns environment secrets,
sessions, server caches and injected server authority. The compiler records the
canonical linked-package set in bytecode and rejects `ui/web/server` when the
target has the bare-browser host surface.

A project may declare `ui.web.toml`:

```toml
routes = ["/", "/articles/wasm-aot", "/offline"]

[document]
language = "en"
direction = "ltr"
title = "Field Notes"
description = "A Volang content site"
canonical_url = "https://example.test"
theme_color = "#315efb"

[[document.assets]]
href = "/site.css"
kind = "style"

[pwa]
enabled = true
name = "Field Notes"
short_name = "Notes"
start_url = "/"
scope = "/"
display = "standalone"
offline_url = "/offline"
cache_version = "field-notes-v1"

[security]
require_https = true
```

`vo ui build . -o dist` validates all routes and policies, executes each route
under its declared location, writes semantic SSR HTML, and emits the selective
activation map next to the server nodes. The output also contains the Core Wasm
AOT image, runtime modules, public assets, PWA manifest, offline service worker,
security headers and `deployment.json`. The deployment manifest describes the
static, Netlify, Cloudflare Pages and object-storage adapter contract without
adding an application JavaScript dependency graph.

SSR is deterministic, escaped, size bounded and UTF-8-safe when streamed. The
DOM host adopts server nodes by generational identity and attaches only the
listeners listed by activation. A failed commit retains the previous revision.
Loaders and actions use explicit request contexts; actions require same-origin
and CSRF checks. Session and cache adapters clone stored values, enforce entry
and byte limits, propagate cancellation and support deterministic tag
invalidation.

The permanent `showcases/content-site` application covers SSG, route-specific
SSR, streaming, metadata, assets, search, forms, activation, PWA and offline
behavior. Its home and article routes are checked in VM and JIT, the release is
lowered to Web AOT, and the result runs in a real browser with zero console
errors.

## Desktop product

`ui/desktop` owns window identities, placement, ownership, lifecycle, monitors,
single-instance handoff, package policy and update policy. The native shell
implements a bounded deterministic lifecycle registry: owned windows close in
reverse creation order, independent renderer scopes survive sibling closure,
and saved geometry is restored against the current monitor set.

A project may declare `ui.desktop.toml`:

```toml
application_id = "dev.volang.data"
name = "Volang Data Application"
version = "0.1.0"
executable = "volang-data"
signing_policy = "optional"
resources = []

[update]
enabled = false
channel = "stable"
endpoint = ""
public_key = ""
```

`vo ui package . -o package` compiles Native AOT and links the static UI runtime.
It emits a macOS application bundle, Windows portable directory or Linux AppDir
according to the target. Platform metadata, resources, update policy and a
SHA-256 inventory live inside the package. Required signing calls the platform
signer; optional signing records the unsigned policy when no identity exists.

Update manifests are Ed25519 authenticated. Source bytes must match their
declared SHA-256, paths remain relative and symlink-free, staging is versioned,
and activation switches atomic current/previous markers. Rollback changes the
active marker while preserving installed versions for recovery.

The permanent data application is packaged as a standalone Native AOT macOS
application during certification and starts its embedded module, native window,
GPU surface and initial revision without a development toolchain or network.

## Ownership and concurrency

Renderer nodes and component state retain one UI writer. Loaders, actions,
cache fills, session work, update downloads and platform requests may run in
component-scoped goroutines. They publish bounded typed completions, carry
cancellation, and can mutate visible state only during the next owned UI turn.
Window closure and component disposal cancel their descendants before cleanup,
which prevents late work from reviving released UI or authority.
