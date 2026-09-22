# Studio startup delivery

Measured on 2026-09-22 with Chromium on the development Apple Silicon machine.
These are local delivery measurements, not a guarantee about public Internet
latency. Each row uses 10 fresh browser contexts. The reported p95 uses nearest
rank, so it is the slowest of these 10 samples.

| Journey | Delivery model | Median | p95 | Acceptance limit |
| --- | --- | ---: | ---: | ---: |
| Cold Docs, enhancement ready | 1 Mbps, 200 ms/request | 755 ms | 784 ms | 1,500 ms |
| Docs to State chapter | Same context and model | — | 334 ms | 1,000 ms |
| Docs to full language specification | Same context and model | — | 828 ms | 1,500 ms |
| Cold root, immediately open State chapter | 1 Mbps, 200 ms/request | 813 ms | 822 ms | 1,500 ms |
| Cold Gallery, real Vo interaction ready | 10 Mbps, 100 ms/request | 2,162 ms | 2,173 ms | 3,000 ms |

The root now serves the lightweight documentation landing page. The root
journey opens the State chapter as soon as its native link is available. Neither
the landing page nor chapter navigation requests a VM or bytecode.
The Docs readiness check includes its enhancement entry, followed by an actual
theme toggle. The Gallery check waits for the VM and increments its real counter.
Chapter navigation reuses browser-cached CSS and JavaScript.

Cold Docs requests total 16,167 bytes of gzip response bodies, including HTML,
CSS and scripts. Its enhancement script graph is 1,967 bytes gzip, checked against
a 20 KiB build limit. No Wasm, bytecode or compiler request occurs on Docs pages.
The fulltext index is a separate request made only after entering a search.
Gallery's initial response bodies total 1,731,252 bytes. Both core runtime files
are requested exactly once with production-like `max-age=600` caching. Request
size records describe complete response bodies; root navigation cancels pending
runtime responses, so their recorded sizes do not represent transferred bytes.

## What changed

- Static Docs retain the native Vo-rendered HTML and omit the hydration snapshot
  and VM boot. A dedicated small entry owns search, clipboard and theme controls.
- Gallery and Playground still use Vo. Navigation into exported Docs loads a
  native document, with one owner per DOM root.
- The root landing page is Docs; Gallery remains an explicit interactive
  destination. Root aliases serve their full target HTML and normalize the URL
  without a second document request. Static internal links use canonical trailing slashes.
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

## Production finding and landing-page correction

The first deployment (`0e49d7c9`, CI 35718755331 and Site 35723082134)
passed complete certification and public-site user journeys. Direct cold Docs
readiness measured 468, 1,279 and 318 ms. Worker resource timing confirmed that
its second access to each preloaded image used cache with zero transfer bytes.

However, a Gallery-first homepage still caused document-navigation stalls in
the measured network. The link was visible after approximately 200 ms and the
Docs request started immediately, but its response headers arrived 12–15 seconds
later while large runtime downloads were outstanding. Removing image preloads
and terminating the Worker did not reliably eliminate the delay in an online
comparison; switching to HTTP/1 also retained a slow case. These measurements
do not identify the particular network intermediary responsible for the queue.

The landing-page correction removes automatic runtime downloads from the public
entry entirely. Its home link and root destination both lead to the documentation
guide; users choose Gallery or Playground explicitly. Tests reject any runtime,
compiler or bytecode request from the root and reading paths. The table above
measures this corrected build. Its production timing must be checked again after
certified deployment.

Gallery still needs its approximately 1.7 MB payload for full interaction. On the
measured production connection, three cold runs took 12,892, 15,628 and 14,546 ms;
at 1 Mbps the payload alone takes over 13 seconds. This change does not claim
two-second full Gallery startup on that connection. Reading the default landing
page and chapters has no dependency on that transfer.
