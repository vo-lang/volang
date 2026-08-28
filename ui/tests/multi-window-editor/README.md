# Multi-window editor contract

Two editors share one Unicode document while retaining independent viewport,
renderer, focus, command, and window identities. Closing either top-level
window leaves the sibling and shared model alive.
