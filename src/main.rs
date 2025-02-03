use {
    args::Nodes,
    notify::{
        RecommendedWatcher,
        Watcher,
    },
    std::{
        cell::Cell,
        collections::HashSet,
        ops::Deref,
        path::Path,
        sync::{
            Arc,
            Mutex,
        },
    },
    workflow::WatchExecStep,
};

pub mod args;
pub mod compiler;
pub mod exec;
pub mod plan;
pub mod reference;
pub mod workflow;

use {
    crate::{
        compiler::Compiler,
        workflow::Workflow,
    },
    anyhow::Result,
    args::{
        InitOutput,
        ManualFormat,
    },
    exec::{
        ExecutionEngine,
        OutputMode,
    },
    std::path::PathBuf,
};

#[tokio::main]
async fn main() -> Result<()> {
    let cmd = crate::args::ClapArgumentLoader::load()?;

    match cmd.command {
        | crate::args::Command::Manual { path, format } => {
            let out_path = PathBuf::from(path);
            std::fs::create_dir_all(&out_path)?;
            match format {
                | ManualFormat::Manpages => {
                    reference::build_manpages(&out_path)?;
                },
                | ManualFormat::Markdown => {
                    reference::build_markdown(&out_path)?;
                },
            }
            Ok(())
        },
        | crate::args::Command::Autocomplete { path, shell } => {
            let out_path = PathBuf::from(path);
            std::fs::create_dir_all(&out_path)?;
            reference::build_shell_completion(&out_path, &shell)?;
            Ok(())
        },
        | crate::args::Command::WorkflowInit { template, output } => {
            match output {
                | InitOutput::File(f) => std::fs::write(f, template.render())?,
                | InitOutput::Stdout => print!("{}", template.render()),
            };
            Ok(())
        },
        | crate::args::Command::WorkflowSchema => {
            print!(
                "{}",
                serde_json::to_string_pretty(&schemars::schema_for!(crate::workflow::Workflow)).unwrap()
            );
            Ok(())
        },
        | crate::args::Command::Execute {
            plan,
            workers,
            no_stdout,
            no_stderr,
        } => {
            let exec_engine = ExecutionEngine::new(OutputMode {
                stdout: !no_stdout,
                stderr: !no_stderr,
            });
            exec_engine.execute(&plan, workers)?;
            Ok(())
        },
        | crate::args::Command::Plan {
            workflow,
            nodes,
            args,
            format,
        } => {
            let w = Workflow::load(&workflow)?;
            let nodes = nodes.select(&w)?;
            let c = Compiler::new(w);
            let x = c.plan(&nodes, &args)?;
            print!("{}", format.serialize(&x)?);
            Ok(())
        },
        | crate::args::Command::List { workflow, format } => {
            let w = Workflow::load(&workflow)?;
            let c = Compiler::new(w);
            c.list(&format).await?;
            Ok(())
        },
        | crate::args::Command::Describe {
            workflow,
            nodes,
            format,
        } => {
            let w = Workflow::load(&workflow)?;
            let nodes = nodes.select(&w)?;
            let c = Compiler::new(w);
            c.describe(&nodes, &format).await?;
            Ok(())
        },
        | crate::args::Command::Multiplex {
            commands,
            program,
            stderr,
            stdout,
            parallelism,
        } => {
            let parallelism = parallelism.unwrap_or(commands.len());
            let result = bobr::multiplexer::Multiplexer::new(program, stderr, commands, parallelism)
                .run()
                .await?;
            if let Some(v) = stdout {
                println!("{}", v.serialize(&result).unwrap());
            }
            Ok(())
        },
        | crate::args::Command::Watch {
            workflow,
            watch,
            args,
            workers,
            root,
        } => {
            let w = Workflow::load(&workflow)?;
            let watch = match &w.watch {
                | Some(v) => {
                    if let Some(v) = v.get(&watch) {
                        v
                    } else {
                        Err(anyhow::anyhow!("no watch node named {} in config", watch))?
                    }
                },
                | None => Err(anyhow::anyhow!("no watch node in config"))?,
            };
            let nodes = match &watch.exec {
                | WatchExecStep::Node { ref_ } => Nodes::Arr(HashSet::<String>::from_iter([ref_.clone()])),
            };
            let nodes = nodes.select(&w)?;
            let regex = fancy_regex::Regex::new(&watch.filter)?;
            let exec_state = Arc::new(if watch.queue {
                None
            } else {
                Some(Mutex::new(Cell::new(false)))
            });
            let c = Compiler::new(w);
            let exec_engine = Arc::new(ExecutionEngine::new(OutputMode {
                stdout: true,
                stderr: true,
            }));
            let trim_path =
                std::fs::canonicalize(&root).unwrap().to_str().unwrap().to_owned() + std::path::MAIN_SEPARATOR_STR;
            let exec_state_callback = exec_state.clone();

            let mut watcher = RecommendedWatcher::new(
                move |result: Result<notify::Event, notify::Error>| {
                    match result {
                        | Ok(e) => {
                            let event_kind = match &e.kind {
                                | notify::EventKind::Create(v) => {
                                    match &v {
                                        | notify::event::CreateKind::Any => "created/any",
                                        | notify::event::CreateKind::File => "created/file",
                                        | notify::event::CreateKind::Folder => "created/folder",
                                        | notify::event::CreateKind::Other => "created/other",
                                    }
                                },
                                | notify::EventKind::Modify(v) => {
                                    match &v {
                                        | notify::event::ModifyKind::Any => "modified/any",
                                        | notify::event::ModifyKind::Data(v) => {
                                            match &v {
                                                | notify::event::DataChange::Any => "modified/data/any",
                                                | notify::event::DataChange::Size => "modified/data/size",
                                                | notify::event::DataChange::Content => "modified/data/content",
                                                | notify::event::DataChange::Other => "modified/data/other",
                                            }
                                        },
                                        | notify::event::ModifyKind::Metadata(v) => {
                                            match &v {
                                                | notify::event::MetadataKind::Any => "modified/metadata/any",
                                                | notify::event::MetadataKind::AccessTime => {
                                                    "modified/metadata/accesstime"
                                                },
                                                | notify::event::MetadataKind::WriteTime => {
                                                    "modified/metadata/writetime"
                                                },
                                                | notify::event::MetadataKind::Permissions => {
                                                    "modified/metadata/permissions"
                                                },
                                                | notify::event::MetadataKind::Ownership => {
                                                    "modified/metadata/ownership"
                                                },
                                                | notify::event::MetadataKind::Extended => "modified/metadata/extended",
                                                | notify::event::MetadataKind::Other => "modified/metadata/other",
                                            }
                                        },
                                        | notify::event::ModifyKind::Name(v) => {
                                            match &v {
                                                | notify::event::RenameMode::Any => "modified/name/any",
                                                | notify::event::RenameMode::To => "modified/name/to",
                                                | notify::event::RenameMode::From => "modified/name/from",
                                                | notify::event::RenameMode::Both => "modified/name/both",
                                                | notify::event::RenameMode::Other => "modified/name/other",
                                            }
                                        },
                                        | notify::event::ModifyKind::Other => "modified/other",
                                    }
                                },
                                | notify::EventKind::Remove(v) => {
                                    match &v {
                                        | notify::event::RemoveKind::Any => "removed/any",
                                        | notify::event::RemoveKind::File => "removed/file",
                                        | notify::event::RemoveKind::Folder => "removed/folder",
                                        | notify::event::RemoveKind::Other => "removed/other",
                                    }
                                },
                                | notify::EventKind::Other => "other",
                                | notify::EventKind::Any => "any",
                                | notify::EventKind::Access(k) => {
                                    match &k {
                                        | notify::event::AccessKind::Any => "access/any",
                                        | notify::event::AccessKind::Read => "access/read",
                                        | notify::event::AccessKind::Open(v) => {
                                            match &v {
                                                | notify::event::AccessMode::Any => "access/open/any",
                                                | notify::event::AccessMode::Execute => "access/open/execute",
                                                | notify::event::AccessMode::Read => "access/open/read",
                                                | notify::event::AccessMode::Write => "access/open/write",
                                                | notify::event::AccessMode::Other => "access/open/other",
                                            }
                                        },
                                        | notify::event::AccessKind::Close(v) => {
                                            match &v {
                                                | notify::event::AccessMode::Any => "access/close/any",
                                                | notify::event::AccessMode::Execute => "access/close/execute",
                                                | notify::event::AccessMode::Read => "access/close/read",
                                                | notify::event::AccessMode::Write => "access/close/write",
                                                | notify::event::AccessMode::Other => "access/close/other",
                                            }
                                        },
                                        | notify::event::AccessKind::Other => "other",
                                    }
                                },
                            };

                            let event_path = e.paths[0].to_str().unwrap().trim_start_matches(&trim_path);
                            let filter = format!("{}|{}", &event_kind, &event_path);
                            if regex.is_match(&filter).unwrap() {
                                match exec_state_callback.deref() {
                                    | Some(v) => {
                                        let state_lock = v.lock().unwrap();
                                        if state_lock.get() {
                                            return;
                                        }
                                        state_lock.set(true);
                                    },
                                    | None => {},
                                }

                                let mut args_new = args.clone();
                                args_new.insert("EVENT".to_owned(), filter);
                                args_new.insert("EVENT_KIND".to_owned(), event_kind.to_owned());
                                args_new.insert("EVENT_PATH".to_owned(), event_path.to_owned());
                                let plan = c.plan(&nodes, &args_new).unwrap();
                                let exec_engine_thread = exec_engine.clone();
                                let state_thread = exec_state_callback.clone();
                                std::thread::spawn(move || {
                                    exec_engine_thread.execute(&plan, workers).unwrap();
                                    match state_thread.deref() {
                                        | Some(v) => {
                                            v.lock().unwrap().set(false);
                                        },
                                        | None => {},
                                    }
                                });
                            }
                        },
                        | Err(e) => {
                            println!("{:?}", e);
                        },
                    }
                },
                notify::Config::default(),
            )?;
            watcher.watch(Path::new(&root), notify::RecursiveMode::Recursive)?;
            loop {}
        },
    }
}
