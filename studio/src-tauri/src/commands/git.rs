use std::process::Command;

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GitResult {
    pub ok: bool,
    pub output: String,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "camelCase", tag = "kind")]
pub enum GitOp {
    #[serde(rename = "git.status")]
    Status,
    #[serde(rename = "git.add")]
    Add { paths: Vec<String> },
    #[serde(rename = "git.commit")]
    Commit { message: String },
    #[serde(rename = "git.push")]
    Push { remote: Option<String>, branch: Option<String> },
    #[serde(rename = "git.pull")]
    Pull { remote: Option<String>, branch: Option<String> },
    #[serde(rename = "git.log")]
    Log { limit: Option<u32> },
    #[serde(rename = "git.diff")]
    Diff { path: Option<String> },
    #[serde(rename = "git.clone")]
    Clone { url: String, dest: String },
    #[serde(rename = "git.branch")]
    Branch { list: Option<bool>, create: Option<String>, delete: Option<String> },
}

#[tauri::command]
pub fn cmd_git_exec(op: GitOp) -> GitResult {
    let args = match &op {
        GitOp::Status => vec!["status".to_string()],
        GitOp::Add { paths } => {
            let mut args = vec!["add".to_string()];
            args.extend(paths.iter().cloned());
            args
        }
        GitOp::Commit { message } => vec!["commit".to_string(), "-m".to_string(), message.clone()],
        GitOp::Push { remote, branch } => {
            let mut args = vec!["push".to_string()];
            if let Some(r) = remote { args.push(r.clone()); }
            if let Some(b) = branch { args.push(b.clone()); }
            args
        }
        GitOp::Pull { remote, branch } => {
            let mut args = vec!["pull".to_string()];
            if let Some(r) = remote { args.push(r.clone()); }
            if let Some(b) = branch { args.push(b.clone()); }
            args
        }
        GitOp::Log { limit } => {
            let n = limit.unwrap_or(20);
            vec!["log".to_string(), format!("-{}", n), "--oneline".to_string()]
        }
        GitOp::Diff { path } => {
            let mut args = vec!["diff".to_string()];
            if let Some(p) = path { args.push(p.clone()); }
            args
        }
        GitOp::Clone { url, dest } => vec!["clone".to_string(), url.clone(), dest.clone()],
        GitOp::Branch { list, create, delete } => {
            let mut args = vec!["branch".to_string()];
            if list.unwrap_or(false) { args.push("-a".to_string()); }
            if let Some(name) = create { args.push(name.clone()); }
            if let Some(name) = delete {
                args.push("-d".to_string());
                args.push(name.clone());
            }
            args
        }
    };
    run_git(&args)
}

fn run_git(args: &[String]) -> GitResult {
    match Command::new("git").args(args).output() {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout).to_string();
            let stderr = String::from_utf8_lossy(&output.stderr).to_string();
            let combined = if stderr.trim().is_empty() {
                stdout
            } else if stdout.trim().is_empty() {
                stderr
            } else {
                format!("{}{}", stdout, stderr)
            };
            GitResult { ok: output.status.success(), output: combined }
        }
        Err(err) => GitResult { ok: false, output: err.to_string() },
    }
}
