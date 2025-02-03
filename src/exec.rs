use {
    crate::plan,
    anyhow::Result,
    std::{
        collections::HashMap,
        process::Stdio,
    },
    threadpool::ThreadPool,
};

#[derive(Debug, Clone)]
pub(crate) struct OutputMode {
    pub stderr: bool,
    pub stdout: bool,
}

pub(crate) struct ExecutionEngine {
    pub output: OutputMode,
}

impl ExecutionEngine {
    pub fn new(output: OutputMode) -> Self {
        Self { output }
    }

    pub fn execute(&self, plan: &plan::ExecutionPlan, workers: usize) -> Result<()> {
        struct Work {
            workdir: Option<String>,
            env: HashMap<String, String>,
            shell: plan::Shell,
            command: String,
        }

        for stage in &plan.stages {
            let pool = ThreadPool::new(workers);
            let (signal_tx, signal_rx) = std::sync::mpsc::channel::<Result<()>>();
            let mut signal_cnt = 0;

            let nodes = stage.nodes.iter().map(|v| plan.nodes.get(v).unwrap());
            for node in nodes {
                for matrix in &node.invocations {
                    let mut work = Vec::<Work>::new();
                    for task in &node.tasks {
                        let workdir = if let Some(workdir) = &task.workdir {
                            Some(workdir.to_owned())
                        } else if let Some(workdir) = &node.workdir {
                            Some(workdir.to_owned())
                        } else {
                            None
                        };

                        let shell = if let Some(shell) = &task.shell {
                            shell.to_owned()
                        } else if let Some(shell) = &node.shell {
                            shell.to_owned()
                        } else {
                            crate::plan::Shell {
                                program: "sh".to_owned(),
                                args: vec!["-c".to_owned()],
                            }
                        };

                        let mut env = plan.env.clone();
                        env.extend(node.env.clone());
                        env.extend(matrix.env.clone());
                        env.extend(task.env.clone());

                        signal_cnt += 1;
                        work.push(Work {
                            command: task.cmd.clone(),
                            env,
                            shell,
                            workdir,
                        })
                    }

                    let output = self.output.clone();
                    // executes matrix entry
                    for w in work {
                        let t_tx = signal_tx.clone();
                        pool.execute(move || {
                            let res = move || -> Result<()> {
                                let mut cmd_proc = std::process::Command::new(w.shell.program);
                                cmd_proc.args(w.shell.args);
                                cmd_proc.envs(w.env);
                                if let Some(w) = w.workdir {
                                    cmd_proc.current_dir(w);
                                }
                                cmd_proc.arg(&w.command);
                                cmd_proc.stdin(Stdio::null());

                                if !output.stdout {
                                    cmd_proc.stdout(Stdio::null());
                                }
                                if !output.stderr {
                                    cmd_proc.stderr(Stdio::null());
                                }

                                let output = cmd_proc.spawn()?.wait_with_output()?;

                                match output.status.code().unwrap() {
                                    | 0 => Ok(()),
                                    | v => {
                                        Err(anyhow::anyhow!(
                                            "command: {} failed to execute with code {}",
                                            w.command,
                                            v
                                        ))
                                    },
                                }?;
                                Ok(())
                            }();
                            t_tx.send(res).expect("send failed");
                        });
                    }
                }
            }

            let errs = signal_rx
                .iter()
                .take(signal_cnt)
                .filter(|x| x.is_err())
                .map(|x| x.expect_err("expecting an err"))
                .map(|v| v.to_string())
                .collect::<Vec<_>>();
            if errs.len() > 0 {
                return Err(anyhow::anyhow!("{}", errs.join("\n")).into());
                // abort at this stage
            }
        }
        Ok(())
    }
}
