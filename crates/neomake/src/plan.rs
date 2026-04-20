use {
    crate::{
        args::ListFormat,
        workflow::{
            ContainerConfig,
            ContainerRuntime,
            ContainerUser,
            RetryPolicy,
            WhenCondition,
            Workflow,
        },
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
    /// Resolved list of producer node names whose stdout is piped to this
    /// node's stdin.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub inputs: Vec<String>,
    /// Node-level retry policy (task-level `Task::retry` takes precedence).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub retry: Option<RetryPolicy>,
}

/// A single matrix invocation: the coordinate vector and the env contribution.
#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Invocation {
    pub cell: Vec<u8>,
    pub env: HashMap<String, String>,
    /// Container config merged across workflow + node + all cells in this
    /// invocation. Used as the base for this invocation's tasks; the
    /// per-task `container` overlays.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub container: Option<ResolvedContainer>,
}

/// Fully-resolved container config baked into the plan — no further merge at
/// execute time.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct ResolvedContainer {
    pub image: String,
    pub workdir: String,
    pub user: ContainerUser,
    pub mount: bool,
    /// Runtime + its extras, inline. See `ContainerRuntime` for the variants.
    pub runtime: ContainerRuntime,
}

/// A rendered task.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Task {
    pub cmd: String,
    pub env: HashMap<String, String>,
    pub shell: Option<String>,
    pub workdir: Option<String>,
    /// Task-level retry policy (overrides the node's if present).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub retry: Option<RetryPolicy>,
    /// Task-level container config, merged over workflow+node+cell.
    /// Takes precedence over `Invocation::container` at execute time.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub container: Option<ResolvedContainer>,
}

/// Merge a chain of optional `ContainerConfig` refs, innermost last. Returns
/// `None` if no field was ever set or if the final `disabled == true`. Returns
/// `Err` if any field is set but `image` is never set (user declared container
/// fields without an image — likely a mistake; lint also catches this at
/// compile time with a better message).
///
/// `extra_args` for each runtime sub-block is **additive** across levels in the
/// order given. Other fields are last-wins.
pub fn merge_container(levels: &[Option<&ContainerConfig>]) -> Result<Option<ResolvedContainer>> {
    use crate::workflow::RuntimeExtras;

    let mut any_field = false;
    let mut image: Option<String> = None;
    let mut workdir: Option<String> = None;
    let mut user: Option<ContainerUser> = None;
    let mut mount: Option<bool> = None;
    let mut disabled: Option<bool> = None;
    // Runtime selection: last-wins for the variant. Extras accumulate only while
    // consecutive layers declare the same variant; switching (e.g. docker →
    // podman) discards the prior variant's extras.
    let mut runtime: Option<ContainerRuntime> = None;

    for level in levels.iter().flatten() {
        let c: &ContainerConfig = *level;
        let declared = c.image.is_some()
            || c.workdir.is_some()
            || c.user.is_some()
            || c.mount.is_some()
            || c.disabled.is_some()
            || c.runtime.is_some();
        if declared {
            any_field = true;
        }
        if let Some(v) = &c.image {
            image = Some(v.clone());
        }
        if let Some(v) = &c.workdir {
            workdir = Some(v.clone());
        }
        if let Some(v) = &c.user {
            user = Some(v.clone());
        }
        if let Some(v) = c.mount {
            mount = Some(v);
        }
        if let Some(v) = c.disabled {
            disabled = Some(v);
        }
        if let Some(new_rt) = &c.runtime {
            runtime = Some(match (runtime.take(), new_rt) {
                | (Some(ContainerRuntime::Docker { docker: a }), ContainerRuntime::Docker { docker: b }) => {
                    let mut ea = a.extra_args;
                    ea.extend(b.extra_args.iter().cloned());
                    ContainerRuntime::Docker {
                        docker: RuntimeExtras { extra_args: ea },
                    }
                },
                | (Some(ContainerRuntime::Podman { podman: a }), ContainerRuntime::Podman { podman: b }) => {
                    let mut ea = a.extra_args;
                    ea.extend(b.extra_args.iter().cloned());
                    ContainerRuntime::Podman {
                        podman: RuntimeExtras { extra_args: ea },
                    }
                },
                // First declaration or a variant switch: take the new runtime as-is.
                | (_, r) => r.clone(),
            });
        }
    }

    if !any_field {
        return Ok(None);
    }
    if disabled == Some(true) {
        return Ok(None);
    }
    let image = image.ok_or_else(|| anyhow!("container config declared but no image resolved"))?;

    Ok(Some(ResolvedContainer {
        image,
        workdir: workdir.unwrap_or_else(|| "/work".to_owned()),
        user: user.unwrap_or_default(),
        mount: mount.unwrap_or(true),
        runtime: runtime.unwrap_or_default(),
    }))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintLevel {
    Error,
    Warn,
}

#[derive(Debug, Clone)]
pub struct LintFinding {
    pub level: LintLevel,
    pub node: String,
    pub message: String,
}

impl LintFinding {
    fn error(node: impl Into<String>, message: String) -> Self {
        Self {
            level: LintLevel::Error,
            node: node.into(),
            message,
        }
    }

    fn warn(node: impl Into<String>, message: String) -> Self {
        Self {
            level: LintLevel::Warn,
            node: node.into(),
            message,
        }
    }
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

        // Evaluate `when` guards first so disabled nodes are removed from the graph
        // before ordering. Guards render with a non-strict handlebars — missing
        // args are falsy, not errors. A node with a falsy guard is skipped
        // entirely.
        let mut guard_hb = handlebars::Handlebars::new();
        guard_hb.set_strict_mode(false);
        let skipped = self.evaluate_guards(&guard_hb, &arg_vals)?;
        let waves = self.determine_order_with_skips(nodes, &skipped)?;

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
                let resolved_inputs = self.resolve_inputs(def, &skipped)?;

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
                    inputs: resolved_inputs,
                    retry: def.retry.clone(),
                };

                // Container merge layers outside the matrix: workflow → node.
                let outer_container: Vec<Option<&ContainerConfig>> =
                    vec![self.workflow.container.as_ref(), def.container.as_ref()];

                // Compile invocations, threading container config through the matrix
                // so each invocation gets its own merged ResolvedContainer.
                node.invocations = match &def.matrix {
                    | Some(m) => m.compile(&outer_container)?,
                    | None => {
                        let container = merge_container(&outer_container)?;
                        vec![Invocation {
                            cell: vec![],
                            env: HashMap::new(),
                            container,
                        }]
                    },
                };

                for task in &def.tasks {
                    let cmd = hb.render_template(&task.script, &arg_vals)?;
                    // Only populate task.container when the task itself declared a
                    // `container` block. Otherwise leave it None so enumerate_batches
                    // falls through to the invocation-level container (which carries
                    // cell-specific overrides).
                    let task_container = if task.container.is_some() {
                        let mut task_layers: Vec<Option<&ContainerConfig>> = outer_container.clone();
                        task_layers.push(task.container.as_ref());
                        merge_container(&task_layers)?
                    } else {
                        None
                    };

                    node.tasks.push(Task {
                        cmd,
                        env: match &task.env {
                            | Some(e) => e.compile()?,
                            | None => HashMap::new(),
                        },
                        shell: task.shell.clone(),
                        workdir: task.workdir.clone(),
                        retry: task.retry.clone().or_else(|| def.retry.clone()),
                        container: task_container,
                    });
                }

                plan.nodes.insert(name.clone(), node);
                stage.nodes.push(name);
            }
            plan.stages.push(stage);
        }

        Ok(plan)
    }

    /// Evaluate every node's `when` guard. Returns the set of node names that
    /// should be omitted from the plan.
    fn evaluate_guards(&self, hb: &handlebars::Handlebars, arg_vals: &serde_json::Value) -> Result<HashSet<String>> {
        let mut skipped = HashSet::<String>::new();
        for (name, def) in &self.workflow.nodes {
            if let Some(guard) = &def.when {
                if !Self::guard_is_truthy(guard, hb, arg_vals)? {
                    skipped.insert(name.clone());
                }
            }
        }
        Ok(skipped)
    }

    fn guard_is_truthy(
        guard: &WhenCondition,
        hb: &handlebars::Handlebars,
        arg_vals: &serde_json::Value,
    ) -> Result<bool> {
        match guard {
            | WhenCondition::Template(s) => {
                let rendered = hb.render_template(s, arg_vals)?;
                let t = rendered.trim();
                Ok(!t.is_empty() && !matches!(t, "false" | "0" | "no"))
            },
            | WhenCondition::Script { script, shell } => {
                let shell = shell.clone().unwrap_or_else(|| "/bin/sh -c".to_owned());
                let mut parts = shell.split_whitespace();
                let program = parts
                    .next()
                    .ok_or_else(|| anyhow!("empty shell specification in `when`"))?;
                let mut cmd = std::process::Command::new(program);
                for p in parts {
                    cmd.arg(p);
                }
                cmd.arg(script)
                    .stdin(std::process::Stdio::null())
                    .stdout(std::process::Stdio::null())
                    .stderr(std::process::Stdio::null());
                let status = cmd.status()?;
                Ok(status.success())
            },
        }
    }

    /// Resolve a node's `inputs` selectors to concrete node names, skipping any
    /// that were guarded out. Errors if an input resolves to a node that
    /// was skipped — depending on something that isn't going to run is a
    /// workflow bug, not silent behavior.
    fn resolve_inputs(&self, def: &crate::workflow::Node, skipped: &HashSet<String>) -> Result<Vec<String>> {
        let mut out = Vec::<String>::new();
        if let Some(inputs) = &def.inputs {
            for sel in inputs {
                let matches = sel.resolve(self.workflow.nodes.keys())?;
                for m in matches {
                    if skipped.contains(&m) {
                        return Err(anyhow!("input \"{}\" was skipped by its `when` guard", m));
                    }
                    if !out.contains(&m) {
                        out.push(m);
                    }
                }
            }
        }
        Ok(out)
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

    /// Static analysis of the workflow. Returns a list of findings. The caller
    /// prints them and exits non-zero if any `Error`-level finding was
    /// produced.
    pub fn lint(&self) -> Result<Vec<LintFinding>> {
        let mut findings = Vec::<LintFinding>::new();
        let node_names: HashSet<&String> = self.workflow.nodes.keys().collect();

        for (name, def) in &self.workflow.nodes {
            // pre / inputs resolution
            for (field, list) in [("pre", def.pre.as_ref()), ("inputs", def.inputs.as_ref())] {
                if let Some(entries) = list {
                    for (idx, sel) in entries.iter().enumerate() {
                        match sel {
                            | crate::workflow::NodeSelector::Name { name: target } => {
                                if !node_names.contains(target) {
                                    findings.push(LintFinding::error(
                                        name,
                                        format!("{field}[{idx}] references missing node \"{target}\""),
                                    ));
                                }
                            },
                            | crate::workflow::NodeSelector::Regex { regex } => {
                                let re = match fancy_regex::Regex::new(regex) {
                                    | Ok(r) => r,
                                    | Err(e) => {
                                        findings.push(LintFinding::error(
                                            name,
                                            format!("{field}[{idx}] invalid regex: {e}"),
                                        ));
                                        continue;
                                    },
                                };
                                let matches = node_names.iter().any(|n| re.is_match(n).unwrap_or(false));
                                if !matches {
                                    findings.push(LintFinding::warn(
                                        name,
                                        format!("{field}[{idx}] regex \"{regex}\" matches no nodes"),
                                    ));
                                }
                            },
                        }
                    }
                }
            }

            // matrix shape
            if let Some(m) = &def.matrix {
                if m.dimensions.iter().any(|d| d.is_empty()) {
                    findings.push(LintFinding::error(name, "matrix has an empty dimension".to_owned()));
                }
                if m.dimensions.is_empty() {
                    findings.push(LintFinding::warn(
                        name,
                        "matrix declared but has zero dimensions (no invocations)".to_owned(),
                    ));
                }
            }

            // env captures compile
            if let Some(env) = &def.env {
                if let Some(rgx) = &env.capture {
                    if let Err(e) = fancy_regex::Regex::new(rgx) {
                        findings.push(LintFinding::error(name, format!("env.capture invalid regex: {e}")));
                    }
                }
            }
            for task in &def.tasks {
                if let Some(env) = &task.env {
                    if let Some(rgx) = &env.capture {
                        if let Err(e) = fancy_regex::Regex::new(rgx) {
                            findings.push(LintFinding::error(name, format!("task env.capture invalid regex: {e}")));
                        }
                    }
                }
            }
        }

        // Workflow-level env capture regex.
        if let Some(env) = &self.workflow.env {
            if let Some(rgx) = &env.capture {
                if let Err(e) = fancy_regex::Regex::new(rgx) {
                    findings.push(LintFinding::error(
                        "<workflow>",
                        format!("env.capture invalid regex: {e}"),
                    ));
                }
            }
        }

        // Cycle detection over the full graph (both pre and inputs are structural
        // edges).
        let all = node_names.iter().map(|s| (*s).clone()).collect::<HashSet<_>>();
        if let Err(e) = self.determine_order(&all) {
            findings.push(LintFinding::error("<graph>", format!("{e}")));
        }

        // Container config validity at every scope.
        Self::lint_container_block("<workflow>", self.workflow.container.as_ref(), &mut findings);
        for (name, def) in &self.workflow.nodes {
            Self::lint_container_block(name, def.container.as_ref(), &mut findings);
            if let Some(matrix) = &def.matrix {
                for (dim_idx, dim) in matrix.dimensions.iter().enumerate() {
                    for (cell_idx, cell) in dim.iter().enumerate() {
                        let scope = format!("{name}.matrix[{dim_idx}][{cell_idx}]");
                        Self::lint_container_block(&scope, cell.container.as_ref(), &mut findings);
                    }
                }
            }
            for (task_idx, task) in def.tasks.iter().enumerate() {
                let scope = format!("{name}.tasks[{task_idx}]");
                Self::lint_container_block(&scope, task.container.as_ref(), &mut findings);
            }
        }

        // Each task must have a resolvable container when any of its merge layers
        // declare one. We attempt the same merge the compiler does and surface
        // image-missing as a lint error for every affected task.
        for (name, def) in &self.workflow.nodes {
            let outer: Vec<Option<&ContainerConfig>> = vec![self.workflow.container.as_ref(), def.container.as_ref()];
            // Check task-scoped merge for every task — stand-alone so a cell-only
            // declaration (which would be caught below) isn't double-reported
            // here.
            for (task_idx, task) in def.tasks.iter().enumerate() {
                let mut layers = outer.clone();
                layers.push(task.container.as_ref());
                if let Err(e) = merge_container(&layers) {
                    findings.push(LintFinding::error(name, format!("tasks[{task_idx}]: {e}")));
                }
            }
            // Check invocation-scoped merge per cell combination, flagging cell-only decls
            // that don't resolve.
            if let Some(matrix) = &def.matrix {
                for (dim_idx, dim) in matrix.dimensions.iter().enumerate() {
                    for (cell_idx, cell) in dim.iter().enumerate() {
                        let mut layers = outer.clone();
                        layers.push(cell.container.as_ref());
                        if let Err(e) = merge_container(&layers) {
                            findings.push(LintFinding::error(name, format!("matrix[{dim_idx}][{cell_idx}]: {e}")));
                        }
                    }
                }
            }
        }

        Ok(findings)
    }

    fn lint_container_block(scope: &str, cfg: Option<&ContainerConfig>, out: &mut Vec<LintFinding>) {
        let Some(cfg) = cfg else {
            return;
        };
        // Reserved args that neomake supplies itself; overriding them is likely a bug.
        const RESERVED: &[&str] = &["-i", "-t", "--rm", "-w", "--workdir", "-v", "--volume"];
        if let Some(runtime) = &cfg.runtime {
            let (name, extras) = match runtime {
                | ContainerRuntime::Docker { docker } => ("docker", docker),
                | ContainerRuntime::Podman { podman } => ("podman", podman),
            };
            for a in &extras.extra_args {
                let bare = a.split('=').next().unwrap_or(a);
                if RESERVED.contains(&bare) {
                    out.push(LintFinding::warn(
                        scope,
                        format!(
                            "container.runtime.{name}.extra_args: `{a}` is controlled by neomake, override may be \
                             ignored"
                        ),
                    ));
                }
            }
        }
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
        self.determine_order_with_skips(exec, &HashSet::new())
    }

    /// Topologically group nodes into waves, omitting anything in `skipped`.
    /// Dependencies of a requested node from both `pre` and `inputs` are
    /// honored.
    fn determine_order_with_skips(
        &self,
        exec: &HashSet<String>,
        skipped: &HashSet<String>,
    ) -> Result<Vec<HashSet<String>>> {
        let mut deps = HashMap::<String, Vec<String>>::new();
        let mut seen = HashSet::<String>::new();
        let mut pending = VecDeque::<String>::new();
        pending.extend(exec.iter().filter(|n| !skipped.contains(*n)).cloned());

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
                    for m in sel.resolve(self.workflow.nodes.keys())? {
                        if !skipped.contains(&m) && !pre_nodes.contains(&m) {
                            pre_nodes.push(m);
                        }
                    }
                }
            }
            // `inputs` imply `pre` — you can't read the stdout of a node that hasn't run.
            if let Some(inputs) = &def.inputs {
                for sel in inputs {
                    for m in sel.resolve(self.workflow.nodes.keys())? {
                        if !skipped.contains(&m) && !pre_nodes.contains(&m) {
                            pre_nodes.push(m);
                        }
                    }
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
