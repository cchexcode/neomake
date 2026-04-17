use {
    crate::{
        args::ListFormat,
        workflow::Workflow,
    },
    anyhow::{
        anyhow,
        Result,
    },
    std::{
        collections::{
            HashMap,
            HashSet,
            VecDeque,
        },
        iter::FromIterator,
    },
};

/// A fully-rendered execution plan, produced by the `plan` subcommand
/// and consumed by `execute`.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct ExecutionPlan {
    pub version: String,
    pub nodes: HashMap<String, Node>,
    pub stages: Vec<Stage>,
    pub env: HashMap<String, String>,
}

/// A single DAG stage (wave). All nodes in a stage are independent and can
/// run in parallel.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Stage {
    pub nodes: Vec<String>,
}

/// A rendered node as it appears in the plan.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Node {
    pub parallel: bool,
    pub invocations: Vec<Invocation>,
    pub tasks: Vec<Task>,
    pub env: HashMap<String, String>,
    pub shell: Option<String>,
    pub workdir: Option<String>,
}

/// A single matrix invocation: the coordinate vector and the env contribution.
#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Invocation {
    pub cell: Vec<u8>,
    pub env: HashMap<String, String>,
}

/// A rendered task.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Task {
    pub cmd: String,
    pub env: HashMap<String, String>,
    pub shell: Option<String>,
    pub workdir: Option<String>,
}

pub struct Compiler {
    pub workflow: Workflow,
}

impl Compiler {
    pub fn new(workflow: Workflow) -> Self {
        Self { workflow }
    }

    /// Build an execution plan for the requested nodes (plus their transitive
    /// prerequisites), ordered into DAG waves.
    pub fn plan(&self, nodes: &HashSet<String>, args: &HashMap<String, String>) -> Result<ExecutionPlan> {
        let mut hb = handlebars::Handlebars::new();
        hb.set_strict_mode(true);
        let arg_vals = self.compile_exec_args(args)?;
        let waves = self.determine_order(nodes)?;

        let mut plan = ExecutionPlan {
            version: env!("CARGO_PKG_VERSION").to_owned(),
            stages: vec![],
            nodes: HashMap::new(),
            env: match &self.workflow.env {
                | Some(e) => e.compile()?,
                | None => HashMap::new(),
            },
        };

        for wave in waves {
            let mut stage = Stage { nodes: vec![] };
            for name in wave {
                let def = &self.workflow.nodes[&name];
                let mut node = Node {
                    parallel: def.matrix.as_ref().map(|m| m.parallel).unwrap_or(false),
                    invocations: vec![],
                    tasks: vec![],
                    env: match &def.env {
                        | Some(e) => e.compile()?,
                        | None => HashMap::new(),
                    },
                    shell: def.shell.clone(),
                    workdir: def.workdir.clone(),
                };

                let default_invocation = vec![Invocation { ..Default::default() }];

                for task in &def.tasks {
                    let cmd = hb.render_template(&task.script, &arg_vals)?;
                    node.tasks.push(Task {
                        cmd,
                        env: match &task.env {
                            | Some(e) => e.compile()?,
                            | None => HashMap::new(),
                        },
                        shell: task.shell.clone(),
                        workdir: task.workdir.clone(),
                    });
                }

                node.invocations = match &def.matrix {
                    | Some(m) => m.compile()?,
                    | None => default_invocation,
                };

                plan.nodes.insert(name.clone(), node);
                stage.nodes.push(name);
            }
            plan.stages.push(stage);
        }

        Ok(plan)
    }

    /// List all nodes. Output is either JSON or a plain custom format.
    pub async fn list(&self, format: &ListFormat) -> Result<()> {
        #[derive(Debug, serde::Serialize)]
        struct Out {
            nodes: Vec<OutNode>,
        }
        #[derive(Debug, serde::Serialize)]
        struct OutNode {
            name: String,
            #[serde(skip_serializing_if = "Option::is_none")]
            description: Option<String>,
            #[serde(skip_serializing_if = "Option::is_none")]
            pre: Option<Vec<String>>,
        }

        let mut info = Out {
            nodes: self
                .workflow
                .nodes
                .iter()
                .map(|(name, def)| {
                    OutNode {
                        name: name.clone(),
                        description: def.description.clone(),
                        pre: def.pre.as_ref().map(|ps| {
                            let mut out = Vec::<String>::new();
                            for sel in ps {
                                if let Ok(matches) = sel.resolve(self.workflow.nodes.keys()) {
                                    out.extend(matches);
                                }
                            }
                            out
                        }),
                    }
                })
                .collect(),
        };
        info.nodes.sort_by(|a, b| a.name.cmp(&b.name));

        match format {
            | ListFormat::Json { pretty } => {
                if *pretty {
                    println!("{}", serde_json::to_string_pretty(&info)?);
                } else {
                    println!("{}", serde_json::to_string(&info)?);
                }
            },
            | ListFormat::Custom => {
                for n in &info.nodes {
                    println!("[{}]", n.name);
                    if let Some(d) = &n.description {
                        println!("{}", d);
                    }
                    if let Some(pre) = &n.pre {
                        println!("prerequisites: [\"{}\"]", pre.join("\", \""));
                    }
                    println!();
                }
            },
        }
        Ok(())
    }

    /// Print DAG waves as JSON.
    pub async fn describe(&self, nodes: &HashSet<String>, pretty: bool) -> Result<()> {
        let waves = self.determine_order(nodes)?;
        #[derive(Debug, serde::Serialize)]
        struct Out {
            stages: Vec<Vec<String>>,
        }
        let info = Out {
            stages: waves.into_iter().map(|w| w.into_iter().collect::<Vec<_>>()).collect(),
        };
        if pretty {
            println!("{}", serde_json::to_string_pretty(&info)?);
        } else {
            println!("{}", serde_json::to_string(&info)?);
        }
        Ok(())
    }

    /// Expand `--arg foo.bar=value` pairs into a nested JSON tree used by
    /// handlebars (so templates may reference `{{ foo.bar }}`).
    fn compile_exec_args(&self, args: &HashMap<String, String>) -> Result<serde_json::Value> {
        fn add(namespace: &mut VecDeque<String>, parent: &mut serde_json::Value, value: &str) {
            let key = namespace.pop_front().unwrap();
            match namespace.len() {
                | 0 => {
                    parent
                        .as_object_mut()
                        .unwrap()
                        .entry(&key)
                        .or_insert(serde_json::Value::String(value.to_owned()));
                },
                | _ => {
                    let child = parent
                        .as_object_mut()
                        .unwrap()
                        .entry(&key)
                        .or_insert(serde_json::Value::Object(serde_json::Map::new()));
                    add(namespace, child, value);
                },
            }
        }

        let mut root = serde_json::Value::Object(serde_json::Map::new());
        for (k, v) in args {
            let parts: Vec<String> = k.split('.').map(|s| s.to_string()).collect();
            let mut q = VecDeque::from(parts);
            add(&mut q, &mut root, v);
        }
        Ok(root)
    }

    /// Topologically group nodes into waves. All nodes within a wave are
    /// independent. Errors on cycles and unknown nodes.
    fn determine_order(&self, exec: &HashSet<String>) -> Result<Vec<HashSet<String>>> {
        let mut deps = HashMap::<String, Vec<String>>::new();
        let mut seen = HashSet::<String>::new();
        let mut pending = VecDeque::<String>::new();
        pending.extend(exec.iter().cloned());

        while let Some(next) = pending.pop_back() {
            if !seen.insert(next.clone()) {
                continue;
            }
            let def = self
                .workflow
                .nodes
                .get(&next)
                .ok_or_else(|| anyhow!("node not found: {}", next))?;
            let mut pre_nodes = Vec::<String>::new();
            if let Some(pre) = &def.pre {
                for sel in pre {
                    pre_nodes.extend(sel.resolve(self.workflow.nodes.keys())?);
                }
            }
            deps.insert(next, pre_nodes.clone());
            pending.extend(pre_nodes);
        }

        let mut settled = HashSet::<String>::new();
        let mut waves = Vec::<HashSet<String>>::new();
        while !deps.is_empty() {
            let leaves = deps
                .iter()
                .filter(|(_, d)| d.iter().all(|x| settled.contains(x)))
                .map(|(k, _)| k.clone())
                .collect::<Vec<_>>();
            if leaves.is_empty() {
                return Err(anyhow!("cycle detected in DAG"));
            }
            for k in &leaves {
                deps.remove(k);
            }
            settled.extend(leaves.iter().cloned());
            waves.push(HashSet::from_iter(leaves));
        }
        Ok(waves)
    }
}
