use {
    anyhow::Result,
    std::path::PathBuf,
};

mod args;
mod exec;
mod multiplex;
mod plan;
mod reference;
mod runner;
mod tui;
mod workflow;

use crate::{
    args::{
        Command,
        ManualFormat,
        WorkflowCommand,
        WorkflowInitOutput,
    },
    plan::Compiler,
    workflow::Workflow,
};

#[tokio::main]
async fn main() -> Result<()> {
    let cmd = crate::args::ClapArgumentLoader::load()?;

    match cmd.command {
        | Command::Manual { path, format } => {
            let out = PathBuf::from(path);
            std::fs::create_dir_all(&out)?;
            match format {
                | ManualFormat::Manpages => reference::build_manpages(&out)?,
                | ManualFormat::Markdown => reference::build_markdown(&out)?,
            }
        },
        | Command::Autocomplete { path, shell } => {
            let out = PathBuf::from(path);
            std::fs::create_dir_all(&out)?;
            reference::build_shell_completion(&out, &shell)?;
        },
        | Command::Workflow(sub) => {
            match sub {
                | WorkflowCommand::Schema => {
                    print!("{}", serde_json::to_string_pretty(&schemars::schema_for!(Workflow))?);
                },
                | WorkflowCommand::Init { template, output } => {
                    match output {
                        | WorkflowInitOutput::File(f) => std::fs::write(f, template.render())?,
                        | WorkflowInitOutput::Stdout => print!("{}", template.render()),
                    }
                },
            }
        },
        | Command::Plan {
            workflow,
            nodes,
            args,
            pretty,
        } => {
            let w = Workflow::load(&workflow)?;
            let selected = nodes.select(&w)?;
            let c = Compiler::new(w);
            let p = c.plan(&selected, &args)?;
            if pretty {
                print!("{}", serde_json::to_string_pretty(&p)?);
            } else {
                print!("{}", serde_json::to_string(&p)?);
            }
        },
        | Command::Execute {
            plan,
            workers,
            no_stdout,
            no_stderr,
            tui,
            summary,
        } => {
            runner::execute(
                plan,
                workers,
                tui,
                runner::PlainOutput {
                    stdout: !no_stdout,
                    stderr: !no_stderr,
                },
                summary,
            )
            .await?;
        },
        | Command::List { workflow, format } => {
            let w = Workflow::load(&workflow)?;
            let c = Compiler::new(w);
            c.list(&format).await?;
        },
        | Command::Lint { workflow, json } => {
            let w = Workflow::load(&workflow)?;
            let c = Compiler::new(w);
            let findings = c.lint()?;
            let exit_code = if findings.iter().any(|f| matches!(f.level, plan::LintLevel::Error)) {
                1
            } else {
                0
            };
            if json {
                let out: Vec<_> = findings
                    .iter()
                    .map(|f| {
                        serde_json::json!({
                            "level": match f.level {
                                plan::LintLevel::Error => "error",
                                plan::LintLevel::Warn => "warn",
                            },
                            "node": f.node,
                            "message": f.message,
                        })
                    })
                    .collect();
                println!("{}", serde_json::to_string_pretty(&out)?);
            } else if findings.is_empty() {
                println!("ok — no findings");
            } else {
                for f in &findings {
                    let tag = match f.level {
                        | plan::LintLevel::Error => "error",
                        | plan::LintLevel::Warn => "warn",
                    };
                    println!("{:<5} [{}] {}", tag, f.node, f.message);
                }
            }
            if exit_code != 0 {
                std::process::exit(exit_code);
            }
        },
        | Command::Describe {
            workflow,
            nodes,
            pretty,
        } => {
            let w = Workflow::load(&workflow)?;
            let selected = nodes.select(&w)?;
            let c = Compiler::new(w);
            c.describe(&selected, pretty).await?;
        },
        | Command::Multiplex {
            commands,
            program,
            stdout_pretty,
            print_stdout,
            parallelism,
            tui,
        } => {
            multiplex::run(program, commands, parallelism, print_stdout, stdout_pretty, tui).await?;
        },
        | Command::Watch {
            filter,
            root,
            program,
            commands,
            parallelism,
        } => {
            multiplex::watch(filter, root, program, commands, parallelism).await?;
        },
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use {
        crate::workflow::Workflow,
        anyhow::Result,
    };

    const WF_MIN: &str = include_str!("../res/templates/min.neomake.conf");
    const WF_MAX: &str = include_str!("../res/templates/max.neomake.conf");
    const WF_PYTHON: &str = include_str!("../res/templates/python.neomake.conf");
    const ALL: &[&str] = &[WF_MIN, WF_MAX, WF_PYTHON];

    #[tokio::test]
    async fn test_parse_workflow() -> Result<()> {
        for src in ALL {
            let _ = Workflow::parse(src)?;
        }
        Ok(())
    }
}
