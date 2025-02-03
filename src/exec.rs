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
        #[derive(Debug)]
        struct Work {
            workdir: Option<String>,
            env: HashMap<String, String>,
            shell: String,
            command: String,
        }

        for stage in &plan.stages {
            let pool = ThreadPool::new(workers);
            let (signal_tx, signal_rx) = std::sync::mpsc::channel::<Result<()>>();
            let mut signal_cnt = 0;

            let nodes = stage.nodes.iter().map(|v| plan.nodes.get(v).unwrap());
            for node in nodes {
                let mut batches = Vec::<Vec<Work>>::new();
                let mut current_batch = Vec::<Work>::new();

                for invoke in &node.invocations {
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
                            "/bin/sh -c".to_owned()
                        };

                        let mut env = plan.env.clone();
                        env.extend(node.env.clone());
                        env.extend(invoke.env.clone());
                        env.extend(task.env.clone());

                        current_batch.push(Work {
                            command: task.cmd.clone(),
                            env,
                            shell,
                            workdir,
                        })
                    }
                }

                // add all items as individual batches if parallel is allowed
                if node.parallel {
                    signal_cnt += current_batch.len();
                    for w in current_batch {
                        batches.push(vec![w]);
                    }
                } else {
                    signal_cnt += 1;
                    batches.push(current_batch);
                }

                let output = self.output.clone();
                // executes matrix entry
                for batch in batches {
                    let t_tx = signal_tx.clone();
                    pool.execute(move || {
                        let res = move || -> Result<()> {
                            for work in batch {
                                let mut shell = work.shell.split_whitespace();
                                let mut cmd_proc = std::process::Command::new(shell.next().unwrap());
                                while let Some(v) = shell.next() {
                                    cmd_proc.arg(v);
                                }
                                cmd_proc.envs(work.env);
                                if let Some(w) = work.workdir {
                                    cmd_proc.current_dir(w);
                                }
                                cmd_proc.arg(&work.command);
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
                                            work.command,
                                            v
                                        ))
                                    },
                                }?;
                            }
                            Ok(())
                        }();
                        t_tx.send(res).expect("send failed");
                    });
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
