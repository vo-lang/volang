// Exercise the language runner's actual process boundary in the lightweight
// engineering test binary, before a Nightly lane builds the language engine.
#[allow(dead_code)]
#[path = "../../vo-test/src/subprocess.rs"]
mod subprocess;
