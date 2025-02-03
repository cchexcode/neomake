use {
    crate::{
        plan,
        workflow::Workflow,
    },
    anyhow::Result,
    std::{
        collections::{
            HashMap,
            HashSet,
            VecDeque,
        },
        iter::FromIterator,
    },
};

pub(crate) struct Compiler {
    pub workflow: Workflow,
}

impl Compiler {
    pub fn new(wf: Workflow) -> Self {
        Self { workflow: wf }
    }

    pub fn plan(&self, nodes: &HashSet<String>, args: &HashMap<String, String>) -> Result<plan::ExecutionPlan> {
        let mut hb = handlebars::Handlebars::new();
        hb.set_strict_mode(true);
        let arg_vals = self.compile_exec_args(args)?;
        let stages = self.determine_order(nodes)?;

        let mut plan = plan::ExecutionPlan {
            stages: vec![],
            nodes: HashMap::<_, _>::new(),
            env: match &self.workflow.env {
                | Some(v) => v.compile()?,
                | None => HashMap::<_, _>::new(),
            },
        };

        for stage in stages {
            let mut rendered_stage = plan::Stage { nodes: vec![] };
            for node in stage {
                let node_def = &self.workflow.nodes[&node];
                let mut rendered_node = plan::Node {
                    invocations: vec![],
                    tasks: vec![],
                    env: match &node_def.env {
                        | Some(v) => v.clone(),
                        | None => HashMap::<_, _>::new(),
                    },
                    shell: match node_def.shell.clone() {
                        | Some(v) => Some(v.into()),
                        | None => None,
                    },
                    workdir: node_def.workdir.clone(),
                };

                // default to one matrix entry
                let invocation_default = vec![crate::plan::Invocation { ..Default::default() }];

                for task in &node_def.tasks {
                    let rendered_cmd = hb.render_template(&task.script, &arg_vals)?;

                    rendered_node.tasks.push(plan::Task {
                        cmd: rendered_cmd,
                        shell: match task.shell.clone() {
                            | Some(v) => Some(v.into()),
                            | None => None,
                        },
                        env: match task.env.clone() {
                            | Some(v) => v,
                            | None => HashMap::<_, _>::new(),
                        },
                        workdir: task.workdir.clone(),
                    });
                }

                rendered_node.invocations = match &node_def.matrix {
                    | Some(m) => m.compile()?,
                    | None => invocation_default,
                };

                plan.nodes.insert(node.clone(), rendered_node);
                rendered_stage.nodes.push(node);
            }
            plan.stages.push(rendered_stage);
        }

        Ok(plan)
    }

    pub async fn list(&self, format: &crate::args::Format) -> Result<()> {
        #[derive(Debug, serde::Serialize)]
        struct Output {
            nodes: Vec<OutputNode>,
        }
        #[derive(Debug, serde::Serialize)]
        struct OutputNode {
            name: String,
            #[serde(skip_serializing_if = "Option::is_none")]
            description: Option<String>,
            #[serde(skip_serializing_if = "Option::is_none")]
            pre: Option<Vec<String>>,
        }

        let mut info = Output {
            nodes: Vec::from_iter(self.workflow.nodes.iter().map(|c| {
                OutputNode {
                    name: c.0.to_owned(),
                    description: c.1.description.clone(),
                    pre: c.1.pre.clone(),
                }
            })),
        };
        info.nodes.sort_by(|a, b| a.name.cmp(&b.name));

        println!("{}", format.serialize(&info)?);

        Ok(())
    }

    pub async fn describe(&self, nodes: &HashSet<String>, format: &crate::args::Format) -> Result<()> {
        let structure = self.determine_order(&nodes)?;

        #[derive(Debug, serde::Serialize)]
        struct Output {
            stages: Vec<Vec<String>>,
        }

        let mut info = Output { stages: Vec::new() };
        for s in structure {
            info.stages
                .push(s.iter().map(|s| s.to_owned()).into_iter().collect::<Vec<_>>());
        }

        println!("{}", format.serialize(&info)?);

        Ok(())
    }

    fn compile_exec_args(&self, args: &HashMap<String, String>) -> Result<serde_json::Value> {
        fn recursive_add(
            namespace: &mut std::collections::VecDeque<String>,
            parent: &mut serde_json::Value,
            value: &str,
        ) {
            let current_namespace = namespace.pop_front().unwrap();
            match namespace.len() {
                | 0 => {
                    parent
                        .as_object_mut()
                        .unwrap()
                        .entry(&current_namespace)
                        .or_insert(serde_json::Value::String(value.to_owned()));
                },
                | _ => {
                    let p = parent
                        .as_object_mut()
                        .unwrap()
                        .entry(&current_namespace)
                        .or_insert(serde_json::Value::Object(serde_json::Map::new()));
                    recursive_add(namespace, p, value);
                },
            }
        }
        let mut values_json = serde_json::Value::Object(serde_json::Map::new());
        for arg in args {
            let namespaces_vec: Vec<String> = arg.0.split('.').map(|s| s.to_string()).collect();
            let mut namespaces = VecDeque::from(namespaces_vec);
            recursive_add(&mut namespaces, &mut values_json, arg.1);
        }
        Ok(values_json)
    }

    fn determine_order(&self, exec: &HashSet<String>) -> Result<Vec<HashSet<String>>> {
        let mut map = HashMap::<String, Vec<String>>::new();

        let mut seen = HashSet::<String>::new();
        let mut pending = VecDeque::<String>::new();
        pending.extend(exec.to_owned());

        while let Some(next) = pending.pop_back() {
            if seen.contains(&next) {
                continue;
            }
            seen.insert(next.clone());

            let c = self.workflow.nodes.get(&next);
            if c.is_none() {
                return Err(anyhow::anyhow!(next));
            }

            if let Some(pre) = &c.unwrap().pre {
                map.insert(next, pre.clone());
                pending.extend(pre.clone());
            } else {
                map.insert(next, Vec::<String>::new());
            }
        }
        seen.clear();

        let mut result = Vec::<HashSet<String>>::new();
        while map.len() > 0 {
            // This implementation SHOULD make use of the unstable hash_drain_filter feature
            // to use the drain_filter method on the hashmap but it's not allowed on stable
            // yet.
            let leafs = map
                .iter()
                .filter_map(|(k, v)| {
                    for v_item in v {
                        if !seen.contains(v_item) {
                            return None;
                        }
                    }
                    Some((k.clone(), v.clone()))
                })
                .collect::<Vec<_>>();
            for v in &leafs {
                map.remove(&v.0);
            }

            if leafs.len() == 0 {
                return Err(anyhow::anyhow!("found recursion in dag"));
            }
            let set = leafs.iter().map(|x| x.0.clone());
            seen.extend(set.clone());
            result.push(HashSet::<String>::from_iter(set));
        }

        Ok(result)
    }
}
