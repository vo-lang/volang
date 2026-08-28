# Volang UI internationalization

The package supplies deterministic locale parsing, generated-message runtime
support, cardinal plural rules, number and date formatting, Unicode-aware text
segmentation, collation keys, bidirectional direction detection, and isolation.

It runs from the same `.vo` source in VM/JIT, Core Wasm AOT, Native AOT, and
server AOT. Host locale data can later be generated into typed catalogs while
the public contracts stay unchanged.
