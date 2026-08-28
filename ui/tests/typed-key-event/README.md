# Typed key and capture-phase command probe

This permanent contract application gives the focused input its own typed
key-down listener and binds a command on its logical ancestor. The command uses
capture phase, so one key executes the scoped command before the target event
can advance the renderer generation. The same ordering must hold through Web
portals and native focus scopes.
