# Volang Field Notes

This permanent E5 showcase exercises multi-route SSG, useful SSR HTML,
selective activation, typed navigation, accessible forms, public assets,
security headers, a Web App Manifest, and an offline service worker.

The permanent product gates cover byte-identical VM/JIT navigation, search,
validation, submission, and route snapshots plus six-route SSR, browser AOT
activation, deep links, deployment metadata, and service-worker registration.

Build it with the repository Web runtime:

```sh
vo ui build ui/showcases/content-site -o /tmp/volang-content-site \
  --runtime-dir=lang/crates/vo-web
```

Run the deterministic application contract with:

```sh
./eng/run-content-site-contracts.sh
```
