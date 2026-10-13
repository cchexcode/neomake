use {
    anyhow::{
        Context,
        Result,
    },
    std::io::IsTerminal,
};

mod args;
mod cancel;
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
            std::fs::create_dir_all(&path).with_context(|| format!("failed to create {}", path.display()))?;
            match format {
                | ManualFormat::Manpages => reference::build_manpages(&path)?,
                | ManualFormat::Markdown => reference::build_markdown(&path)?,
            }
        },
        | Command::Autocomplete { path, shell } => {
            std::fs::create_dir_all(&path).with_context(|| format!("failed to create {}", path.display()))?;
            reference::build_shell_completion(&path, &shell)?;
        },
        | Command::Workflow(sub) => {
            match sub {
                | WorkflowCommand::Schema => {
                    print!("{}", serde_json::to_string_pretty(&schemars::schema_for!(Workflow))?);
                },
                | WorkflowCommand::Init { template, output } => {
                    match output {
                        | WorkflowInitOutput::File(path) => {
                            std::fs::write(&path, template.render())
                                .with_context(|| format!("failed to write {}", path.display()))?;
                        },
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
            let host = std::env::vars().collect();
            let w = Workflow::load(&workflow)?;
            let selected = nodes.select(&w)?;
            let p = Compiler::new(w).plan(&selected, &args, &host)?;
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
                workers.get(),
                tui.requested() && std::io::stderr().is_terminal(),
                runner::PlainOutput {
                    stdout: !no_stdout,
                    stderr: !no_stderr,
                },
                summary,
            )
            .await?;
        },
        | Command::List { workflow, format } => {
            let listing = Compiler::new(Workflow::load(&workflow)?).list()?;
            match format {
                | crate::args::ListFormat::Json { pretty } => {
                    let rendered = if pretty {
                        serde_json::to_string_pretty(&listing)?
                    } else {
                        serde_json::to_string(&listing)?
                    };
                    println!("{rendered}");
                },
                | crate::args::ListFormat::Custom => {
                    for node in &listing.nodes {
                        println!("[{}]", node.name);
                        if let Some(description) = &node.description {
                            println!("{description}");
                        }
                        if let Some(pre) = &node.pre {
                            println!("prerequisites: [\"{}\"]", pre.join("\", \""));
                        }
                        println!();
                    }
                },
            }
        },
        | Command::Lint { workflow, json } => {
            let findings = Compiler::new(Workflow::load(&workflow)?).lint()?;
            if json {
                println!("{}", serde_json::to_string_pretty(&findings)?);
            } else if findings.is_empty() {
                println!("ok — no findings");
            } else {
                for finding in &findings {
                    println!("{:<5} [{}] {}", finding.level, finding.node, finding.message);
                }
            }
            if findings.iter().any(|finding| finding.level == plan::LintLevel::Error) {
                std::process::exit(1);
            }
        },
        | Command::Describe {
            workflow,
            nodes,
            pretty,
        } => {
            let w = Workflow::load(&workflow)?;
            let selected = nodes.select(&w)?;
            let waves = Compiler::new(w).describe(&selected)?;
            let rendered = if pretty {
                serde_json::to_string_pretty(&waves)?
            } else {
                serde_json::to_string(&waves)?
            };
            println!("{rendered}");
        },
        | Command::Multiplex {
            commands,
            program,
            stdout_pretty,
            print_stdout,
            parallelism,
            tui,
        } => {
            multiplex::run(
                program,
                commands,
                parallelism.map(std::num::NonZeroUsize::get),
                print_stdout,
                stdout_pretty,
                tui.requested() && std::io::stderr().is_terminal(),
            )
            .await?;
        },
        | Command::Watch {
            filter,
            root,
            program,
            commands,
            parallelism,
        } => {
            multiplex::watch(filter, root, program, commands, parallelism.get()).await?;
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
