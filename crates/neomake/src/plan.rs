use {
    crate::workflow::{
        ContainerConfig,
        ContainerRuntime,
        ContainerUser,
        RetryPolicy,
        RuntimeExtras,
        WhenCondition,
        Workflow,
    },
    anyhow::{
        anyhow,
        bail,
        Context,
        Result,
    },
    std::{
        collections::{
            BTreeMap,
            BTreeSet,
            HashMap,
            HashSet,
            VecDeque,
        },
        fmt,
    },
};

/// Fully resolved execution plan. Produced by `plan`, consumed by `execute`.
///
/// Every task already has its shell, env, retry, and container. Execute does
/// not merge. A plan from a different neomake version is rejected.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct ExecutionPlan {
    pub version: String,
    pub stages: Vec<Stage>,
    pub nodes: BTreeMap<String, PlannedNode>,
}

/// One DAG wave. Nodes in a wave have no edges between them.
///
/// `nodes` is sorted. Order inside a wave does not affect execution.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Stage {
    pub nodes: Vec<String>,
}

/// A node after matrix expansion and guard evaluation.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct PlannedNode {
    pub parallel: bool,
    /// Direct scheduling edges (`pre` plus `inputs`), sorted and de-duplicated.
    #[serde(default)]
    pub deps: Vec<String>,
    /// Producer nodes whose stdout is delivered on stdin, in declaration order.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub inputs: Vec<String>,
    pub invocations: Vec<PlannedInvocation>,
}

/// One matrix cell, or the single invocation of a node without a matrix.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct PlannedInvocation {
    /// Per-dimension indices. Empty for a non-matrix node.
    pub cell: Vec<u32>,
    pub tasks: Vec<PlannedTask>,
}

/// One command, fully resolved for one invocation.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct PlannedTask {
    pub cmd: String,
    pub shell: ShellSpec,
    pub env: BTreeMap<String, String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub workdir: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub retry: Option<RetryPolicy>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub container: Option<ResolvedContainer>,
}

/// A non-empty shell invocation such as `/bin/sh -c`.
///
/// Stored as the original string so plans stay readable. Deserialization
/// rejects an empty program so a hand-edited plan cannot smuggle one through.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShellSpec(String);

impl ShellSpec {
    pub fn sh() -> Self {
        Self("/bin/sh -c".to_owned())
    }

    pub fn parse(shell: impl Into<String>) -> Result<Self> {
        let shell = shell.into();
        if shell.split_whitespace().next().is_none() {
            bail!("empty shell specification");
        }
        Ok(Self(shell))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Program and the arguments that precede the script.
    pub fn split(&self) -> Result<(&str, Vec<&str>)> {
        let mut parts = self.as_str().split_whitespace();
        let program = parts.next().ok_or_else(|| anyhow!("empty shell specification"))?;
        Ok((program, parts.collect()))
    }
}

impl serde::Serialize for ShellSpec {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.0)
    }
}

impl<'de> serde::Deserialize<'de> for ShellSpec {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        Self::parse(s).map_err(serde::de::Error::custom)
    }
}

/// Container config after the workflow → node → cell → task merge.
/// Execute uses this as-is.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct ResolvedContainer {
    pub image: String,
    pub workdir: String,
    pub user: ContainerUser,
    pub mount: bool,
    pub runtime: ContainerRuntime,
}

/// Merge container layers, innermost last.
///
/// `None` if no field was set, or if the final `disabled` is true. Errors when
/// a field is set but no image resolves. `extra_args` accumulate only while the
/// runtime variant stays the same; a switch discards the previous variant's
/// extras.
pub(crate) fn merge_container(levels: &[Option<&ContainerConfig>]) -> Result<Option<ResolvedContainer>> {
    let mut any_field = false;
    let mut image: Option<String> = None;
    let mut workdir: Option<String> = None;
    let mut user: Option<ContainerUser> = None;
    let mut mount: Option<bool> = None;
    let mut disabled: Option<bool> = None;
    let mut runtime: Option<ContainerRuntime> = None;

    for level in levels.iter().flatten().copied() {
        let declared = level.image.is_some()
            || level.workdir.is_some()
            || level.user.is_some()
            || level.mount.is_some()
            || level.disabled.is_some()
            || level.runtime.is_some();
        if declared {
            any_field = true;
        }
        if let Some(v) = &level.image {
            image = Some(v.clone());
        }
        if let Some(v) = &level.workdir {
            workdir = Some(v.clone());
        }
        if let Some(v) = &level.user {
            user = Some(v.clone());
        }
        if let Some(v) = level.mount {
            mount = Some(v);
        }
        if let Some(v) = level.disabled {
            disabled = Some(v);
        }
        if let Some(new_rt) = &level.runtime {
            runtime = Some(merge_runtime(runtime.take(), new_rt));
        }
    }

    if !any_field || disabled == Some(true) {
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

fn merge_runtime(prev: Option<ContainerRuntime>, new_rt: &ContainerRuntime) -> ContainerRuntime {
    match (prev, new_rt) {
        | (Some(ContainerRuntime::Docker { docker: a }), ContainerRuntime::Docker { docker: b }) => {
            ContainerRuntime::Docker {
                docker: RuntimeExtras {
                    extra_args: extend_args(a.extra_args, &b.extra_args),
                },
            }
        },
        | (Some(ContainerRuntime::Podman { podman: a }), ContainerRuntime::Podman { podman: b }) => {
            ContainerRuntime::Podman {
                podman: RuntimeExtras {
                    extra_args: extend_args(a.extra_args, &b.extra_args),
                },
            }
        },
        | (_, rt) => rt.clone(),
    }
}

fn extend_args(mut base: Vec<String>, extra: &[String]) -> Vec<String> {
    base.extend(extra.iter().cloned());
    base
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum LintLevel {
    Error,
    Warn,
}

impl fmt::Display for LintLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            | Self::Error => "error",
            | Self::Warn => "warn",
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct LintFinding {
    pub level: LintLevel,
    pub node: String,
    pub message: String,
}

impl LintFinding {
    fn error(node: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            level: LintLevel::Error,
            node: node.into(),
            message: message.into(),
        }
    }

    fn warn(node: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            level: LintLevel::Warn,
            node: node.into(),
            message: message.into(),
        }
    }
}

#[derive(Debug, serde::Serialize)]
pub struct NodeList {
    pub nodes: Vec<ListedNode>,
}

#[derive(Debug, serde::Serialize)]
pub struct ListedNode {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pre: Option<Vec<String>>,
}

#[derive(Debug, serde::Serialize)]
pub struct WaveList {
    pub stages: Vec<Vec<String>>,
}

pub struct Compiler {
    workflow: Workflow,
}

impl Compiler {
    pub fn new(workflow: Workflow) -> Self {
        Self { workflow }
    }

    /// Compile `nodes` plus their transitive dependencies into a plan.
    ///
    /// `host` is the process environment captured by the caller. Capture
    /// regexes read that map and nothing else. `when` script guards are the
    /// one plan-time side effect: they spawn a process and include the node
    /// on exit 0.
    pub fn plan(
        &self,
        nodes: &HashSet<String>,
        args: &HashMap<String, String>,
        host: &HashMap<String, String>,
    ) -> Result<ExecutionPlan> {
        let mut hb = handlebars::Handlebars::new();
        hb.set_strict_mode(true);
        let arg_vals = compile_exec_args(args)?;

        let mut guard_hb = handlebars::Handlebars::new();
        guard_hb.set_strict_mode(false);
        let skipped = self.evaluate_guards(&guard_hb, &arg_vals)?;
        let mut requested_skipped: Vec<_> = nodes.iter().filter(|n| skipped.contains(*n)).cloned().collect();
        requested_skipped.sort();
        if !requested_skipped.is_empty() {
            bail!(
                "requested node(s) skipped by `when` guard: {}",
                requested_skipped.join(", ")
            );
        }

        let ordered = self.order(nodes, &skipped)?;
        let mut plan = ExecutionPlan {
            version: env!("CARGO_PKG_VERSION").to_owned(),
            stages: Vec::new(),
            nodes: BTreeMap::new(),
        };

        for wave in ordered.waves {
            let mut stage = Stage { nodes: Vec::new() };
            for name in wave {
                let def = &self.workflow.nodes[&name];
                let inputs = self.resolve_inputs(def, &skipped)?;
                let deps = ordered.deps.get(&name).cloned().unwrap_or_default();
                let invocations = self.compile_invocations(def, &name, &hb, &arg_vals, host)?;
                plan.nodes.insert(name.clone(), PlannedNode {
                    parallel: def.matrix.as_ref().map(|m| m.parallel).unwrap_or(false),
                    deps,
                    inputs,
                    invocations,
                });
                stage.nodes.push(name);
            }
            plan.stages.push(stage);
        }
        Ok(plan)
    }

    fn compile_invocations(
        &self,
        def: &crate::workflow::Node,
        name: &str,
        hb: &handlebars::Handlebars,
        arg_vals: &serde_json::Value,
        host: &HashMap<String, String>,
    ) -> Result<Vec<PlannedInvocation>> {
        let workflow_env = compile_env(self.workflow.env.as_ref(), host)?;
        let node_env = compile_env(def.env.as_ref(), host)?;
        let points = match &def.matrix {
            | Some(m) => m.expand().with_context(|| format!("node `{name}`"))?,
            | None => {
                vec![crate::workflow::MatrixPoint {
                    cell: vec![],
                    parts: vec![],
                }]
            },
        };

        let mut invocations = Vec::with_capacity(points.len());
        for point in points {
            let mut cell_env = HashMap::<String, String>::new();
            let mut cell_containers = Vec::new();
            for part in &point.parts {
                if let Some(env) = &part.env {
                    cell_env.extend(env.compile(host)?);
                }
                cell_containers.push(part.container.as_ref());
            }

            let mut tasks = Vec::with_capacity(def.tasks.len());
            for (task_idx, task) in def.tasks.iter().enumerate() {
                let cmd = hb
                    .render_template(&task.script, arg_vals)
                    .with_context(|| format!("node `{name}` task {task_idx} template"))?;
                let shell = match task.shell.as_ref().or(def.shell.as_ref()) {
                    | Some(s) => ShellSpec::parse(s).with_context(|| format!("node `{name}` task {task_idx} shell"))?,
                    | None => ShellSpec::sh(),
                };
                let mut env = BTreeMap::new();
                env.extend(workflow_env.iter().map(|(k, v)| (k.clone(), v.clone())));
                env.extend(node_env.iter().map(|(k, v)| (k.clone(), v.clone())));
                env.extend(cell_env.iter().map(|(k, v)| (k.clone(), v.clone())));
                if let Some(task_env) = &task.env {
                    env.extend(task_env.compile(host)?);
                }

                let mut layers = vec![self.workflow.container.as_ref(), def.container.as_ref()];
                layers.extend(cell_containers.iter().copied());
                layers.push(task.container.as_ref());
                let container = merge_container(&layers).with_context(|| format!("node `{name}` task {task_idx}"))?;

                tasks.push(PlannedTask {
                    cmd,
                    shell,
                    env,
                    workdir: task.workdir.clone().or_else(|| def.workdir.clone()),
                    retry: task.retry.clone().or_else(|| def.retry.clone()),
                    container,
                });
            }
            invocations.push(PlannedInvocation {
                cell: point.cell,
                tasks,
            });
        }
        Ok(invocations)
    }

    /// `when` guards. A falsy guard omits the node. Script guards spawn a
    /// process.
    fn evaluate_guards(&self, hb: &handlebars::Handlebars, arg_vals: &serde_json::Value) -> Result<HashSet<String>> {
        let mut skipped = HashSet::new();
        for (name, def) in &self.workflow.nodes {
            if let Some(guard) = &def.when {
                let include =
                    guard_is_truthy(guard, hb, arg_vals).with_context(|| format!("node `{name}` when guard failed"))?;
                if !include {
                    skipped.insert(name.clone());
                }
            }
        }
        Ok(skipped)
    }

    fn resolve_inputs(&self, def: &crate::workflow::Node, skipped: &HashSet<String>) -> Result<Vec<String>> {
        let mut out = Vec::new();
        if let Some(inputs) = &def.inputs {
            for sel in inputs {
                for matched in sel.resolve(self.workflow.nodes.keys())? {
                    if skipped.contains(&matched) {
                        bail!("input \"{matched}\" was skipped by its `when` guard");
                    }
                    if !out.contains(&matched) {
                        out.push(matched);
                    }
                }
            }
        }
        Ok(out)
    }

    /// Nodes in name order, with resolved `pre` selectors. Selector errors
    /// propagate.
    pub fn list(&self) -> Result<NodeList> {
        let mut nodes = Vec::new();
        for (name, def) in &self.workflow.nodes {
            let pre = match &def.pre {
                | Some(selectors) => {
                    let mut out = Vec::new();
                    for sel in selectors {
                        for matched in sel.resolve(self.workflow.nodes.keys())? {
                            if !out.contains(&matched) {
                                out.push(matched);
                            }
                        }
                    }
                    Some(out)
                },
                | None => None,
            };
            nodes.push(ListedNode {
                name: name.clone(),
                description: def.description.clone(),
                pre,
            });
        }
        nodes.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(NodeList { nodes })
    }

    /// Static analysis. The caller exits non-zero on any error-level finding.
    pub fn lint(&self) -> Result<Vec<LintFinding>> {
        let mut findings = Vec::new();
        let node_names: HashSet<&String> = self.workflow.nodes.keys().collect();

        for (name, def) in &self.workflow.nodes {
            for (field, list) in [("pre", def.pre.as_ref()), ("inputs", def.inputs.as_ref())] {
                let Some(entries) = list else {
                    continue;
                };
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
                            match fancy_regex::Regex::new(regex) {
                                | Ok(re) => {
                                    let matches = node_names.iter().any(|n| re.is_match(n).unwrap_or(false));
                                    if !matches {
                                        findings.push(LintFinding::warn(
                                            name,
                                            format!("{field}[{idx}] regex \"{regex}\" matches no nodes"),
                                        ));
                                    }
                                },
                                | Err(e) => {
                                    findings
                                        .push(LintFinding::error(name, format!("{field}[{idx}] invalid regex: {e}")))
                                },
                            }
                        },
                    }
                }
            }

            if let Some(matrix) = &def.matrix {
                if matrix.dimensions.is_empty() {
                    findings.push(LintFinding::warn(
                        name,
                        "matrix has zero dimensions; treated as a single invocation",
                    ));
                } else if let Err(e) = matrix.expand() {
                    findings.push(LintFinding::error(name, e.to_string()));
                }
            }

            lint_shell(name, def.shell.as_deref(), &mut findings);
            if let Some(WhenCondition::Script { shell: Some(shell), .. }) = &def.when {
                lint_shell(name, Some(shell), &mut findings);
            }
            lint_env_capture(name, "env.capture", def.env.as_ref(), &mut findings);
            for (task_idx, task) in def.tasks.iter().enumerate() {
                lint_shell(name, task.shell.as_deref(), &mut findings);
                lint_env_capture(
                    name,
                    &format!("tasks[{task_idx}].env.capture"),
                    task.env.as_ref(),
                    &mut findings,
                );
            }
        }

        lint_env_capture("<workflow>", "env.capture", self.workflow.env.as_ref(), &mut findings);
        lint_container_block("<workflow>", self.workflow.container.as_ref(), &mut findings);
        for (name, def) in &self.workflow.nodes {
            lint_container_block(name, def.container.as_ref(), &mut findings);
            if let Some(matrix) = &def.matrix {
                for (dim_idx, dim) in matrix.dimensions.iter().enumerate() {
                    for (cell_idx, cell) in dim.iter().enumerate() {
                        lint_container_block(
                            &format!("{name}.matrix[{dim_idx}][{cell_idx}]"),
                            cell.container.as_ref(),
                            &mut findings,
                        );
                    }
                }
            }
            for (task_idx, task) in def.tasks.iter().enumerate() {
                lint_container_block(
                    &format!("{name}.tasks[{task_idx}]"),
                    task.container.as_ref(),
                    &mut findings,
                );
            }
            self.lint_resolved_containers(name, def, &mut findings);
        }

        let all = node_names.iter().map(|s| (*s).clone()).collect::<HashSet<_>>();
        if let Err(e) = self.order(&all, &HashSet::new()) {
            findings.push(LintFinding::error("<graph>", e.to_string()));
        }
        Ok(findings)
    }

    fn lint_resolved_containers(&self, name: &str, def: &crate::workflow::Node, findings: &mut Vec<LintFinding>) {
        let cell_sets: Vec<Vec<Option<ContainerConfig>>> = match &def.matrix {
            | None => vec![Vec::new()],
            | Some(matrix) => {
                match matrix.expand() {
                    | Ok(points) => {
                        points
                            .into_iter()
                            .map(|point| point.parts.into_iter().map(|cell| cell.container.clone()).collect())
                            .collect()
                    },
                    | Err(_) => return,
                }
            },
        };
        for (task_idx, task) in def.tasks.iter().enumerate() {
            for cells in &cell_sets {
                let mut layers = vec![self.workflow.container.as_ref(), def.container.as_ref()];
                layers.extend(cells.iter().map(Option::as_ref));
                layers.push(task.container.as_ref());
                if let Err(e) = merge_container(&layers) {
                    findings.push(LintFinding::error(name, format!("tasks[{task_idx}]: {e}")));
                    break;
                }
            }
        }
    }

    /// DAG waves for `nodes` plus transitive `pre` and `inputs`. Does not apply
    /// guards.
    pub fn describe(&self, nodes: &HashSet<String>) -> Result<WaveList> {
        Ok(WaveList {
            stages: self.order(nodes, &HashSet::new())?.waves,
        })
    }

    fn order(&self, roots: &HashSet<String>, skipped: &HashSet<String>) -> Result<Ordered> {
        let mut deps = BTreeMap::<String, BTreeSet<String>>::new();
        let mut seen = BTreeSet::new();
        let mut pending = VecDeque::new();
        pending.extend(roots.iter().filter(|n| !skipped.contains(*n)).cloned());

        while let Some(next) = pending.pop_front() {
            if !seen.insert(next.clone()) {
                continue;
            }
            let def = self
                .workflow
                .nodes
                .get(&next)
                .ok_or_else(|| anyhow!("node not found: {next}"))?;
            let mut pre = BTreeSet::new();
            for selectors in [&def.pre, &def.inputs] {
                let Some(selectors) = selectors else {
                    continue;
                };
                for sel in selectors {
                    for matched in sel.resolve(self.workflow.nodes.keys())? {
                        if !skipped.contains(&matched) {
                            pre.insert(matched);
                        }
                    }
                }
            }
            pending.extend(pre.iter().cloned());
            deps.insert(next, pre);
        }

        let mut remaining = deps.clone();
        let mut settled = BTreeSet::new();
        let mut waves = Vec::new();
        while !remaining.is_empty() {
            let leaves: Vec<String> = remaining
                .iter()
                .filter(|(_, edges)| edges.iter().all(|edge| settled.contains(edge)))
                .map(|(name, _)| name.clone())
                .collect();
            if leaves.is_empty() {
                let stuck = remaining.keys().cloned().collect::<Vec<_>>().join(", ");
                bail!("cycle detected in DAG: {stuck}");
            }
            for name in &leaves {
                remaining.remove(name);
            }
            settled.extend(leaves.iter().cloned());
            waves.push(leaves);
        }

        let deps = deps
            .into_iter()
            .map(|(name, edges)| (name, edges.into_iter().collect()))
            .collect();
        Ok(Ordered { waves, deps })
    }
}

struct Ordered {
    waves: Vec<Vec<String>>,
    deps: BTreeMap<String, Vec<String>>,
}

fn compile_env(env: Option<&crate::workflow::Env>, host: &HashMap<String, String>) -> Result<HashMap<String, String>> {
    match env {
        | Some(env) => env.compile(host),
        | None => Ok(HashMap::new()),
    }
}

fn lint_shell(node: &str, shell: Option<&str>, findings: &mut Vec<LintFinding>) {
    if let Some(shell) = shell {
        if ShellSpec::parse(shell).is_err() {
            findings.push(LintFinding::error(node, format!("empty shell specification `{shell}`")));
        }
    }
}

fn lint_env_capture(node: &str, field: &str, env: Option<&crate::workflow::Env>, findings: &mut Vec<LintFinding>) {
    let Some(regex) = env.and_then(|e| e.capture.as_ref()) else {
        return;
    };
    if let Err(e) = fancy_regex::Regex::new(regex) {
        findings.push(LintFinding::error(node, format!("{field} invalid regex: {e}")));
    }
}

fn lint_container_block(scope: &str, cfg: Option<&ContainerConfig>, out: &mut Vec<LintFinding>) {
    let Some(cfg) = cfg else {
        return;
    };
    const RESERVED: &[&str] = &["-i", "-t", "--rm", "-w", "--workdir", "-v", "--volume"];
    let Some(runtime) = &cfg.runtime else {
        return;
    };
    let (name, extras) = match runtime {
        | ContainerRuntime::Docker { docker } => ("docker", docker),
        | ContainerRuntime::Podman { podman } => ("podman", podman),
    };
    for arg in &extras.extra_args {
        let bare = arg.split('=').next().unwrap_or(arg);
        if RESERVED.contains(&bare) {
            out.push(LintFinding::warn(
                scope,
                format!(
                    "container.runtime.{name}.extra_args: `{arg}` is controlled by neomake; override may be ignored"
                ),
            ));
        }
    }
}

fn guard_is_truthy(guard: &WhenCondition, hb: &handlebars::Handlebars, arg_vals: &serde_json::Value) -> Result<bool> {
    match guard {
        | WhenCondition::Template(template) => {
            let rendered = hb.render_template(template, arg_vals)?;
            let trimmed = rendered.trim();
            Ok(!trimmed.is_empty() && !matches!(trimmed, "false" | "0" | "no"))
        },
        | WhenCondition::Script { script, shell } => {
            let spec = match shell {
                | Some(shell) => ShellSpec::parse(shell)?,
                | None => ShellSpec::sh(),
            };
            let (program, args) = spec.split()?;
            let mut cmd = std::process::Command::new(program);
            for arg in args {
                cmd.arg(arg);
            }
            let status = cmd
                .arg(script)
                .stdin(std::process::Stdio::null())
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .status()
                .with_context(|| format!("failed to run `when` script `{script}`"))?;
            Ok(status.success())
        },
    }
}

/// Expand `key=value` pairs into the JSON tree handlebars reads.
///
/// `a.b=1` becomes `{ "a": { "b": "1" } }`. Empty segments and a path that is
/// both a value and an object are errors — those plans would otherwise render
/// the wrong argument or panic.
fn compile_exec_args(args: &HashMap<String, String>) -> Result<serde_json::Value> {
    let mut root = serde_json::Map::new();
    let mut keys: Vec<_> = args.keys().collect();
    keys.sort();
    for key in keys {
        insert_arg(&mut root, key, &args[key])?;
    }
    Ok(serde_json::Value::Object(root))
}

fn insert_arg(root: &mut serde_json::Map<String, serde_json::Value>, key: &str, value: &str) -> Result<()> {
    if key.is_empty() || key.split('.').any(|part| part.is_empty()) {
        bail!("arg key `{key}` is empty or has an empty segment");
    }
    let parts: Vec<&str> = key.split('.').collect();
    insert_at(root, &parts, value, key)
}

fn insert_at(
    node: &mut serde_json::Map<String, serde_json::Value>,
    parts: &[&str],
    value: &str,
    full: &str,
) -> Result<()> {
    let Some((head, rest)) = parts.split_first() else {
        bail!("arg key `{full}` is empty");
    };
    if rest.is_empty() {
        match node.get(*head) {
            | Some(serde_json::Value::String(existing)) if existing == value => Ok(()),
            | Some(_) => bail!("arg `{full}` conflicts with an earlier arg"),
            | None => {
                node.insert((*head).to_owned(), serde_json::Value::String(value.to_owned()));
                Ok(())
            },
        }
    } else {
        let entry = node
            .entry((*head).to_owned())
            .or_insert_with(|| serde_json::Value::Object(serde_json::Map::new()));
        match entry {
            | serde_json::Value::Object(map) => insert_at(map, rest, value, full),
            | _ => bail!("arg `{full}` conflicts with an earlier arg"),
        }
    }
}

/// Longest chain through `deps`, weighted by per-node duration.
///
/// Ties prefer the lexicographically later end node, then the earlier sorted
/// dependency, so the path is stable. Returns `None` when every weight is 0.
pub(crate) fn critical_path(plan: &ExecutionPlan, weight: &HashMap<String, u128>) -> Option<(Vec<String>, u128)> {
    let mut best = BTreeMap::<String, (u128, Option<String>)>::new();
    for stage in &plan.stages {
        for name in &stage.nodes {
            let Some(node) = plan.nodes.get(name) else {
                continue;
            };
            let w = weight.get(name).copied().unwrap_or(0);
            let mut chosen = (w, None);
            for dep in &node.deps {
                if let Some((dep_total, _)) = best.get(dep) {
                    let candidate = dep_total.saturating_add(w);
                    if candidate > chosen.0 {
                        chosen = (candidate, Some(dep.clone()));
                    }
                }
            }
            best.insert(name.clone(), chosen);
        }
    }
    let (end, (total, _)) = best.iter().max_by_key(|(_, (total, _))| *total)?;
    if *total == 0 {
        return None;
    }
    let mut path = Vec::new();
    let mut cur = Some(end.clone());
    while let Some(name) = cur {
        path.push(name.clone());
        cur = best.get(&name).and_then(|(_, parent)| parent.clone());
    }
    path.reverse();
    Some((path, *total))
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::workflow::{
            ContainerConfig,
            ContainerRuntime,
            RuntimeExtras,
        },
    };

    fn dedent(src: &str) -> String {
        let lines: Vec<&str> = src.lines().filter(|line| !line.trim().is_empty()).collect();
        let indent = lines
            .iter()
            .map(|line| line.len() - line.trim_start().len())
            .min()
            .unwrap_or(0);
        lines
            .into_iter()
            .map(|line| line.get(indent..).unwrap_or(line.trim()))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn plan_src(src: &str, nodes: &[&str], args: &[(&str, &str)], host: &[(&str, &str)]) -> Result<ExecutionPlan> {
        let wf = Workflow::parse(&dedent(src))?;
        let selected = nodes.iter().map(|s| (*s).to_owned()).collect();
        let args = args.iter().map(|(k, v)| ((*k).to_owned(), (*v).to_owned())).collect();
        let host = host.iter().map(|(k, v)| ((*k).to_owned(), (*v).to_owned())).collect();
        Compiler::new(wf).plan(&selected, &args, &host)
    }

    #[test]
    fn plan_resolves_env_container_and_is_stable() {
        let src = r#"
            version = "0.0"
            env {
              capture = "^CAPTURED$"
              vars {
                K = "wf"
                SHARED = "wf"
              }
            }
            container {
              image = "alpine:base"
              runtime {
                docker {
                  extra_args = [ "--from=wf" ]
                }
              }
            }
            nodes {
              build {
                env {
                  vars { K = "node" }
                }
                matrix {
                  parallel = true
                  dimensions = [
                    [
                      {
                        env {
                          vars { K = "cell" }
                        }
                        container {
                          image = "debian:cell"
                          runtime {
                            docker {
                              extra_args = [ "--from=cell" ]
                            }
                          }
                        }
                      }
                    ]
                  ]
                }
                tasks = [
                  {
                    script = "echo {{ args.msg }}"
                    env {
                      vars {
                        K = "task"
                        ONLY_TASK = "1"
                      }
                    }
                    container {
                      workdir = "/app"
                    }
                  }
                ]
              }
            }
        "#;
        let host = [("CAPTURED", "yes"), ("OTHER", "no")];
        let plan = plan_src(src, &["build"], &[("args.msg", "hi")], &host).unwrap();
        let again = plan_src(src, &["build"], &[("args.msg", "hi")], &host).unwrap();
        assert_eq!(plan, again);

        let task = &plan.nodes["build"].invocations[0].tasks[0];
        assert_eq!(task.cmd, "echo hi");
        assert_eq!(task.shell.as_str(), "/bin/sh -c");
        assert_eq!(task.env.get("K").map(String::as_str), Some("task"));
        assert_eq!(task.env.get("SHARED").map(String::as_str), Some("wf"));
        assert_eq!(task.env.get("ONLY_TASK").map(String::as_str), Some("1"));
        assert_eq!(task.env.get("CAPTURED").map(String::as_str), Some("yes"));
        assert!(!task.env.contains_key("OTHER"));
        let container = task.container.as_ref().unwrap();
        assert_eq!(container.image, "debian:cell");
        assert_eq!(container.workdir, "/app");
        assert_eq!(container.user, ContainerUser::Host);
        match &container.runtime {
            | ContainerRuntime::Docker { docker } => {
                assert_eq!(docker.extra_args, vec![
                    "--from=wf".to_owned(),
                    "--from=cell".to_owned()
                ]);
            },
            | ContainerRuntime::Podman { .. } => panic!("expected docker"),
        }
    }

    #[test]
    fn runtime_switch_discards_previous_extras_and_disable_opts_out() {
        let switched = plan_src(
            r#"
                version = "0.0"
                container {
                  image = "alpine"
                  runtime {
                    docker {
                      extra_args = [ "--from=docker" ]
                    }
                  }
                }
                nodes {
                  t {
                    tasks = [
                      {
                        script = "true"
                        container {
                          runtime {
                            podman {
                              extra_args = [ "--from=podman" ]
                            }
                          }
                        }
                      }
                    ]
                  }
                }
            "#,
            &["t"],
            &[],
            &[],
        )
        .unwrap();
        match &switched.nodes["t"].invocations[0].tasks[0]
            .container
            .as_ref()
            .unwrap()
            .runtime
        {
            | ContainerRuntime::Podman { podman } => assert_eq!(podman.extra_args, vec!["--from=podman".to_owned()]),
            | ContainerRuntime::Docker { .. } => panic!("expected podman"),
        }

        let disabled = plan_src(
            r#"
                version = "0.0"
                container {
                  image = "alpine"
                }
                nodes {
                  t {
                    container {
                      disabled = true
                    }
                    tasks = [
                      { script = "true" }
                    ]
                  }
                }
            "#,
            &["t"],
            &[],
            &[],
        )
        .unwrap();
        assert!(disabled.nodes["t"].invocations[0].tasks[0].container.is_none());
    }

    #[test]
    fn container_user_literal_round_trips_through_hocon() {
        let plan = plan_src(
            r#"
                version = "0.0"
                container {
                  image = "alpine"
                  user = "1000:1000"
                }
                nodes {
                  t {
                    tasks = [
                      { script = "true" }
                    ]
                  }
                }
            "#,
            &["t"],
            &[],
            &[],
        )
        .unwrap();
        assert_eq!(
            plan.nodes["t"].invocations[0].tasks[0].container.as_ref().unwrap().user,
            ContainerUser::Explicit("1000:1000".to_owned())
        );
    }

    #[test]
    fn guards_cycles_args_and_wave_order() {
        let err = plan_src(
            r#"
                version = "0.0"
                nodes {
                  a {
                    when = "false"
                    tasks = [
                      { script = "true" }
                    ]
                  }
                }
            "#,
            &["a"],
            &[],
            &[],
        )
        .unwrap_err();
        assert!(err.to_string().contains("skipped by"), "{err}");

        let plan = plan_src(
            r#"
                version = "0.0"
                nodes {
                  a {
                    when = "false"
                    tasks = [
                      { script = "true" }
                    ]
                  }
                  b {
                    pre = [
                      { name = "a" }
                    ]
                    tasks = [
                      { script = "true" }
                    ]
                  }
                }
            "#,
            &["b"],
            &[],
            &[],
        )
        .unwrap();
        assert_eq!(plan.stages.len(), 1);
        assert_eq!(plan.stages[0].nodes, vec!["b".to_owned()]);
        assert!(plan.nodes["b"].deps.is_empty());

        let err = plan_src(
            r#"
                version = "0.0"
                nodes {
                  a {
                    when = "false"
                    tasks = [
                      { script = "echo x" }
                    ]
                  }
                  b {
                    inputs = [
                      { name = "a" }
                    ]
                    tasks = [
                      { script = "cat" }
                    ]
                  }
                }
            "#,
            &["b"],
            &[],
            &[],
        )
        .unwrap_err();
        assert!(err.to_string().contains("skipped"), "{err}");

        let err = plan_src(
            r#"
                version = "0.0"
                nodes {
                  a {
                    pre = [
                      { name = "b" }
                    ]
                    tasks = [
                      { script = "true" }
                    ]
                  }
                  b {
                    pre = [
                      { name = "a" }
                    ]
                    tasks = [
                      { script = "true" }
                    ]
                  }
                }
            "#,
            &["a"],
            &[],
            &[],
        )
        .unwrap_err();
        assert!(err.to_string().contains("cycle detected"), "{err}");

        let err = plan_src(
            r#"
                version = "0.0"
                nodes {
                  a {
                    tasks = [
                      { script = "echo {{ args.foo }}" }
                    ]
                  }
                }
            "#,
            &["a"],
            &[("foo", "1"), ("foo.bar", "2")],
            &[],
        )
        .unwrap_err();
        assert!(err.to_string().contains("conflicts"), "{err}");

        let plan = plan_src(
            r#"
                version = "0.0"
                nodes {
                  b {
                    tasks = [
                      { script = "true" }
                    ]
                  }
                  a {
                    tasks = [
                      { script = "true" }
                    ]
                  }
                }
            "#,
            &["b", "a"],
            &[],
            &[],
        )
        .unwrap();
        assert_eq!(plan.stages[0].nodes, vec!["a".to_owned(), "b".to_owned()]);
    }

    #[test]
    fn missing_image_and_empty_shell_fail_closed() {
        assert!(plan_src(
            r#"
                version = "0.0"
                nodes {
                  a {
                    container {
                      workdir = "/x"
                    }
                    tasks = [
                      { script = "true" }
                    ]
                  }
                }
            "#,
            &["a"],
            &[],
            &[],
        )
        .is_err());
        assert!(plan_src(
            r#"
                version = "0.0"
                nodes {
                  a {
                    shell = "   "
                    tasks = [
                      { script = "true" }
                    ]
                  }
                }
            "#,
            &["a"],
            &[],
            &[],
        )
        .is_err());
    }

    #[test]
    fn merge_container_rules() {
        let base = ContainerConfig {
            image: Some("alpine".to_owned()),
            runtime: Some(ContainerRuntime::Docker {
                docker: RuntimeExtras {
                    extra_args: vec!["--a".to_owned()],
                },
            }),
            ..ContainerConfig::default()
        };
        let switched = ContainerConfig {
            runtime: Some(ContainerRuntime::Podman {
                podman: RuntimeExtras {
                    extra_args: vec!["--b".to_owned()],
                },
            }),
            ..ContainerConfig::default()
        };
        let resolved = merge_container(&[Some(&base), Some(&switched)]).unwrap().unwrap();
        assert_eq!(resolved.image, "alpine");
        assert!(matches!(resolved.runtime, ContainerRuntime::Podman { .. }));

        let disabled = ContainerConfig {
            image: Some("alpine".to_owned()),
            disabled: Some(true),
            ..ContainerConfig::default()
        };
        assert!(merge_container(&[Some(&disabled)]).unwrap().is_none());
        assert!(merge_container(&[]).unwrap().is_none());
        let bare = ContainerConfig {
            workdir: Some("/x".to_owned()),
            ..ContainerConfig::default()
        };
        assert!(merge_container(&[Some(&bare)]).is_err());
    }

    #[test]
    fn critical_path_follows_deps_not_just_inputs() {
        let plan = ExecutionPlan {
            version: "0.0.0".to_owned(),
            stages: vec![
                Stage {
                    nodes: vec!["a".to_owned(), "b".to_owned()],
                },
                Stage {
                    nodes: vec!["c".to_owned()],
                },
            ],
            nodes: BTreeMap::from([
                ("a".to_owned(), PlannedNode {
                    parallel: false,
                    deps: vec![],
                    inputs: vec![],
                    invocations: vec![],
                }),
                ("b".to_owned(), PlannedNode {
                    parallel: false,
                    deps: vec![],
                    inputs: vec![],
                    invocations: vec![],
                }),
                ("c".to_owned(), PlannedNode {
                    parallel: false,
                    deps: vec!["a".to_owned(), "b".to_owned()],
                    inputs: vec![],
                    invocations: vec![],
                }),
            ]),
        };
        let weights = HashMap::from([("a".to_owned(), 10u128), ("b".to_owned(), 3), ("c".to_owned(), 4)]);
        let (path, total) = critical_path(&plan, &weights).unwrap();
        assert_eq!(path, vec!["a".to_owned(), "c".to_owned()]);
        assert_eq!(total, 14);
    }

    #[test]
    fn templates_have_no_lint_errors() {
        for src in [
            include_str!("../res/templates/min.neomake.conf"),
            include_str!("../res/templates/max.neomake.conf"),
            include_str!("../res/templates/python.neomake.conf"),
        ] {
            let findings = Compiler::new(Workflow::parse(src).unwrap()).lint().unwrap();
            let errors: Vec<_> = findings.iter().filter(|f| f.level == LintLevel::Error).collect();
            assert!(errors.is_empty(), "{errors:?}");
        }
    }
}
