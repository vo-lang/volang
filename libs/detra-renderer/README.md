# Detra Renderer

Detra Renderer is the platform rendering engine for Detra UI trees. It converts the platform-agnostic UI Tree into a concrete UI, keeps node identity via keys, and forwards user input as Detra action calls.

## Responsibilities
- Maintain renderer state and diff/patch UI trees.
- Convert platform events into action calls.
- Preserve node identity using keys (focus/scroll/animation continuity).

## Public Vo API
- Run(config Config, onAction func(action detra.ActionCall))
- Render(tree detra.Node)

See renderer.vo for the Vo interface.

## Build (Rust)
From repo root:
- cargo build -p detra_renderer

The shared library is produced at target/debug/libdetra_renderer.so.
