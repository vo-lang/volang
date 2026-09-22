# Studio startup delivery

Measured on 2026-09-22 with Chromium on the development Apple Silicon machine.
These are local delivery measurements, not a guarantee about public Internet
latency. Each row uses 10 fresh browser contexts. The reported p95 uses nearest
rank, so it is the slowest of these 10 samples.

| Journey | Delivery model | Median | p95 | Acceptance limit |
| --- | --- | ---: | ---: | ---: |
| Cold Docs, enhancement ready | 1 Mbps, 200 ms/request | 769 ms | 777 ms | 1,500 ms |
| Docs to State chapter | Same context and model | — | 344 ms | 1,000 ms |
| Docs to full language specification | Same context and model | — | 806 ms | 1,500 ms |
| Cold root, immediately click Documentation | 1 Mbps, 200 ms/request | 1,204 ms | 1,255 ms | 1,500 ms |
| Cold Gallery, real Vo interaction ready | 10 Mbps, 100 ms/request | 2,167 ms | 2,254 ms | 3,000 ms |

The root journey starts interacting as soon as its HTML and visible native link
are available. It does not wait for the Gallery module graph or VM to finish.
The Docs readiness check includes its enhancement entry, followed by an actual
theme toggle. The Gallery check waits for the VM and increments its real counter.
Chapter navigation reuses browser-cached CSS and JavaScript.

Cold Docs requests total 16,166 bytes of gzip response bodies, including HTML,
CSS and scripts. Its enhancement script graph is 1,967 bytes gzip, checked against
a 20 KiB build limit. No Wasm, bytecode or compiler request occurs on Docs pages.
The fulltext index is a separate request made only after entering a search.
Gallery's initial response bodies total 1,731,241 bytes. Both core runtime files
are requested exactly once with production-like `max-age=600` caching. Request
size records describe complete response bodies; root navigation cancels pending
runtime responses, so their recorded sizes do not represent transferred bytes.

## What changed

- Static Docs retain the native Vo-rendered HTML and omit the hydration snapshot
  and VM boot. A dedicated small entry owns search, clipboard and theme controls.
- Gallery and Playground still use Vo. Navigation into exported Docs loads a
  native document, with one owner per DOM root.
- Root aliases serve their full target HTML and normalize the URL without a
  second document request. Static internal links use canonical trailing slashes.
- The Studio Worker bundles its runtime bindings and synchronous host imports.
  Interactive HTML preloads the build-derived boot graph, Wasm and compressed
  bytecode. Compiler and editor loading remain on their existing intent paths.

## Validation and reproduction

Run `VO_TEST_PROFILE=release node eng/ui-next/cli.mjs build --studio --static`,
then `node eng/ui-next/studio-startup-benchmark.mjs`. The benchmark writes all
30 samples and request records to `target/ui-next/startup-benchmark.json` and
fails if any journey's p95 exceeds the table's limit. Its HTTP server applies
round-robin shared response-byte bandwidth, gzip, a fixed per-request delay and
production-like cache headers. DNS, TLS, packet loss, CPU throttling and external
network variability are outside this model.

Validated locally:

- Chromium, Firefox and WebKit: complete relocated static Studio, editor and
  language-service journeys; content-only search, retry, copy, history, theme,
  mobile layout and blocked runtime resources.
- Three-browser compressed resource delivery, SSR early input and lazy workers.
- Chromium Node distribution: full Vo Docs, nested routes, SSR adoption, drafts
  and execution. Static content checks do not replace these framework checks.
- 175 toolchain unit tests, repository lint, static certification evidence,
  candidate file identities and deployment size budgets.

The previously observed 13–24 second online stalls included runtime transfer.
The new design removes that dependency from document reading. Gallery still
needs its approximately 1.7 MB runtime payload for full interaction; at 1 Mbps
that payload alone takes over 13 seconds. This change does not claim two-second
full Gallery startup at that bandwidth. Public-site timing must be measured
again after the certified deployment.
