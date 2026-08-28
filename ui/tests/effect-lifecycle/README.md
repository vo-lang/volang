# Effect lifecycle probe

This E2 contract application verifies that an external effect starts after its
declaring renderer commit. Removing the nested component invalidates its state
generation, cancels the effect context, and only then invokes cleanup. The
visible `ordering violation` state makes an ordering regression executable in
VM, JIT, Core Wasm AOT, and Native AOT certification.
