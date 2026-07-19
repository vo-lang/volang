use std::collections::BTreeSet;

pub(crate) fn inferred_tools_for_command(command: &[String]) -> BTreeSet<&'static str> {
    let mut tools = BTreeSet::new();
    let Some(binary) = command.first().map(String::as_str) else {
        return tools;
    };
    match binary {
        "cargo" => {
            tools.insert("rust");
            if cargo_run_invokes_vo_dev(command) {
                tools.insert("vo-dev");
            }
        }
        "node" => {
            tools.insert("node");
        }
        "npm" => {
            tools.extend(["node", "npm"]);
        }
        "wasm-pack" => {
            tools.extend(["rust", "wasm-pack"]);
        }
        "python" | "python3" => {
            tools.insert("python");
        }
        "./d.py" => {
            tools.extend(["python", "rust", "vo-dev"]);
        }
        "vo-dev" => {
            tools.insert("vo-dev");
        }
        _ => {}
    }
    if let Some(args) = vo_dev_invocation_args(command) {
        infer_vo_dev_subcommand_tools(args, &mut tools);
    }
    tools
}

fn vo_dev_invocation_args(command: &[String]) -> Option<&[String]> {
    if command.first().map(String::as_str) == Some("vo-dev") {
        return Some(&command[1..]);
    }
    if !cargo_run_invokes_vo_dev(command) {
        return None;
    }
    let separator = command.iter().position(|arg| arg == "--")?;
    Some(&command[separator + 1..])
}

fn cargo_run_invokes_vo_dev(command: &[String]) -> bool {
    if command.first().map(String::as_str) != Some("cargo") {
        return false;
    }
    let args_end = command
        .iter()
        .position(|arg| arg == "--")
        .unwrap_or(command.len());
    let args = &command[1..args_end];
    if !args.iter().any(|arg| arg == "run") {
        return false;
    }
    args.windows(2)
        .any(|pair| matches!(pair[0].as_str(), "-p" | "--package" | "--bin") && pair[1] == "vo-dev")
        || args.iter().any(|arg| {
            matches!(
                arg.as_str(),
                "-pvo-dev" | "-p=vo-dev" | "--package=vo-dev" | "--bin=vo-dev"
            )
        })
}

fn infer_vo_dev_subcommand_tools(args: &[String], tools: &mut BTreeSet<&'static str>) {
    match args {
        [command, ..] if command == "studio-install-local-vogui" => {
            tools.extend(["node", "npm"]);
        }
        [first, second, _repo, _location, rest @ ..]
            if first == "first-party" && matches!(second.as_str(), "run" | "run-workspace") =>
        {
            if let Some(separator) = rest.iter().position(|arg| arg == "--") {
                tools.extend(inferred_tools_for_command(&rest[separator + 1..]));
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::inferred_tools_for_command;

    #[test]
    fn infers_nested_first_party_tools() {
        let command = [
            "vo-dev",
            "first-party",
            "run-workspace",
            "vogui",
            "js",
            "--",
            "npm",
            "run",
            "build",
        ]
        .map(str::to_string);
        let tools = inferred_tools_for_command(&command);
        assert_eq!(tools, ["node", "npm", "vo-dev"].into_iter().collect());
    }
}
