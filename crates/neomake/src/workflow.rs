use {
    anyhow::{
        anyhow,
        Result,
    },
    itertools::Itertools,
    std::{
        collections::HashMap,
        path::Path,
    },
};

/// Top-level workflow definition. Loaded from a HOCON file.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case")]
pub struct Workflow {
    /// The workflow file schema version (major.minor).
    pub version: String,
    /// Global environment variable configuration.
    pub env: Option<Env>,
    /// Workflow-wide default container encapsulation (see `ContainerConfig`).
    pub container: Option<ContainerConfig>,
    /// All DAG nodes.
    pub nodes: HashMap<String, Node>,
}

impl Workflow {
    /// Load and validate a workflow from a HOCON file. `include "other.conf"`
    /// directives are resolved relative to the workflow file's directory.
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self> {
        let wf: Workflow = hocon::HoconLoader::new()
            .load_file(path.as_ref())
            .map_err(|e| anyhow!("failed to read workflow {}: {}", path.as_ref().display(), e))?
            .resolve()
            .map_err(|e| anyhow!("failed to deserialize workflow: {}", e))?;
        Self::validate(wf)
    }

    /// Parse and validate a workflow from a HOCON string. Note: `include`
    /// directives are disabled when parsing from a string (the hocon crate
    /// needs a base path). Use `Workflow::load` for files with includes.
    #[cfg(test)]
    pub fn parse(src: &str) -> Result<Self> {
        let wf: Workflow = hocon::HoconLoader::new()
            .load_str(src)
            .map_err(|e| anyhow!("failed to parse HOCON: {}", e))?
            .resolve()
            .map_err(|e| anyhow!("failed to deserialize workflow: {}", e))?;
        Self::validate(wf)
    }

    fn validate(wf: Workflow) -> Result<Self> {
        let major_minor = env!("CARGO_PKG_VERSION").split('.').take(2).join(".");
        if major_minor != "0.0" && wf.version != major_minor {
            return Err(anyhow!(
                "workflow version {} is incompatible with CLI version {}",
                wf.version,
                env!("CARGO_PKG_VERSION")
            ));
        }

        let name_re = fancy_regex::Regex::new(r"^[a-zA-Z0-9:_-]+$")?;
        for n in wf.nodes.keys() {
            if !name_re.is_match(n)? {
                return Err(anyhow!("invalid node name: {}", n));
            }
        }
        Ok(wf)
    }
}

/// Environment variable configuration: capture host env vars by regex,
/// and/or supply explicit key/value pairs.
#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Env {
    /// Regex of host env vars to capture at compile time.
    pub capture: Option<String>,
    /// Explicit env vars.
    pub vars: Option<HashMap<String, String>>,
}

impl Env {
    /// Compile into the concrete map of env vars.
    pub fn compile(&self) -> Result<HashMap<String, String>> {
        let mut map = self.vars.clone().unwrap_or_default();
        if let Some(regex) = &self.capture {
            let re = fancy_regex::Regex::new(regex)?;
            for (k, v) in std::env::vars() {
                if re.is_match(&k)? {
                    map.insert(k, v);
                }
            }
        }
        Ok(map)
    }
}

/// A node selector. Used in `pre`: either by exact name or regex.
///
/// Structurally either `{ name = "..." }` or `{ regex = "..." }` in HOCON.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(untagged, deny_unknown_fields)]
pub enum NodeSelector {
    Name { name: String },
    Regex { regex: String },
}

impl NodeSelector {
    /// Return the matched node names in this workflow for this selector.
    pub fn resolve<'a, I: IntoIterator<Item=&'a String>>(&self, candidates: I) -> Result<Vec<String>> {
        match self {
            | NodeSelector::Name { name } => Ok(vec![name.clone()]),
            | NodeSelector::Regex { regex } => {
                let re = fancy_regex::Regex::new(regex)?;
                let mut out = Vec::<String>::new();
                for k in candidates {
                    if re.is_match(k)? {
                        out.push(k.clone());
                    }
                }
                Ok(out)
            },
        }
    }
}

/// A single DAG node, holding a list of tasks and optional matrix.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Node {
    /// Optional description shown in `list`.
    pub description: Option<String>,
    /// Prerequisite nodes that must execute before this one (ordering only).
    pub pre: Option<Vec<NodeSelector>>,
    /// Input producers whose stdout is streamed to this node's stdin on spawn.
    /// Each producer listed here is also an implicit `pre` dependency. The
    /// consumer receives a single JSON blob keyed by producer node name;
    /// see the README for the exact shape.
    pub inputs: Option<Vec<NodeSelector>>,
    /// Optional conditional guard. If evaluated false at plan time, the node
    /// (and any nodes that depend only on it) is dropped from the plan
    /// entirely.
    pub when: Option<WhenCondition>,
    /// Node-level retry policy (task-level policies override this).
    pub retry: Option<RetryPolicy>,
    /// Optional n-dimensional invocation matrix.
    pub matrix: Option<Matrix>,
    /// The tasks to be executed.
    pub tasks: Vec<Task>,
    /// Node-level env.
    pub env: Option<Env>,
    /// Node-level shell (e.g. "/bin/bash -c").
    pub shell: Option<String>,
    /// Node-level workdir.
    pub workdir: Option<String>,
    /// Node-level container encapsulation (overrides workflow-level fields by
    /// merge).
    pub container: Option<ContainerConfig>,
}

/// Conditional guard on a node. Two shapes:
///   - `when = "{{ args.env }}"` — handlebars template; truthy result includes
///     the node.
///   - `when { script = "...", shell = "..." }` — runs at plan time; exit 0
///     includes.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(untagged, deny_unknown_fields)]
pub enum WhenCondition {
    Template(String),
    Script {
        /// Shell script; exit code 0 means "include this node".
        script: String,
        /// Optional shell override (defaults to "/bin/sh -c").
        #[serde(default)]
        shell: Option<String>,
    },
}

/// Per-task retry policy.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct RetryPolicy {
    /// Total attempts (including the first try). `1` disables retrying.
    pub attempts: u32,
    /// Backoff strategy. Defaults to `fixed`.
    #[serde(default)]
    pub backoff: Backoff,
    /// Initial delay between attempts, in milliseconds. Defaults to 500.
    #[serde(default = "default_delay_ms")]
    pub delay_ms: u64,
}

fn default_delay_ms() -> u64 {
    500
}

#[derive(Debug, Clone, Copy, Default, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum Backoff {
    /// `delay_ms` between every retry.
    #[default]
    Fixed,
    /// `delay_ms * attempt` — 1x, 2x, 3x, …
    Linear,
    /// `delay_ms * 2^(attempt-1)` — 1x, 2x, 4x, …
    Exponential,
}

/// Optional container encapsulation for a task. May be declared at four scopes
/// (workflow, node, matrix cell, task) and is merged innermost-wins; see the
/// compiler's `merge_container` helper. A task runs containerized iff the
/// merged result has `image` set and `disabled != true`.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct ContainerConfig {
    /// OCI image reference, e.g. `"alpine:latest"`. Required at the merged
    /// level.
    pub image: Option<String>,
    /// Working directory inside the container. Defaults to `/work`.
    pub workdir: Option<String>,
    /// User mapping — see `ContainerUser`. Defaults to `Host`.
    pub user: Option<ContainerUser>,
    /// Bind-mount the host CWD at `workdir`. Defaults to true.
    pub mount: Option<bool>,
    /// Opt out at a lower scope. If a lower scope sets `disabled = true`, no
    /// container runs.
    pub disabled: Option<bool>,
    /// Runtime selection. An `{ docker { … } }` or `{ podman { … } }` block
    /// that also carries its runtime-specific extras. Defaults to `docker`
    /// with empty extras if no merge level declares it.
    pub runtime: Option<ContainerRuntime>,
}

/// Runtime-specific options for a container. Each enum variant is a runtime
/// (`docker` / `podman`); the struct inside carries runtime-specific knobs —
/// today just `extra_args`. Declared untagged so the HOCON form is the
/// ergonomic `runtime { docker { … } }` shape.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(untagged, deny_unknown_fields)]
pub enum ContainerRuntime {
    Docker {
        #[serde(default)]
        docker: RuntimeExtras,
    },
    Podman {
        #[serde(default)]
        podman: RuntimeExtras,
    },
}

impl Default for ContainerRuntime {
    fn default() -> Self {
        ContainerRuntime::Docker {
            docker: RuntimeExtras::default(),
        }
    }
}

impl ContainerRuntime {
    /// Reference to this runtime's `RuntimeExtras`.
    pub fn extras(&self) -> &RuntimeExtras {
        match self {
            | ContainerRuntime::Docker { docker } => docker,
            | ContainerRuntime::Podman { podman } => podman,
        }
    }
}

/// Runtime-specific knobs. Currently just `extra_args`; additive across merge
/// levels as long as consecutive levels declare the same runtime variant.
/// Switching runtimes between layers discards the prior variant's extras.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct RuntimeExtras {
    /// Extra args appended to `<runtime> run`.
    #[serde(default)]
    pub extra_args: Vec<String>,
}

/// User mapping for the container's primary process.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub enum ContainerUser {
    /// Match the host user: `--user $(id -u):$(id -g)`. Default.
    Host,
    /// Run as root — no `--user` flag added.
    Root,
    /// Literal argument passed verbatim to `--user` (e.g. `"1000:1000"` or
    /// `"appuser"`).
    Explicit(String),
}

impl Default for ContainerUser {
    fn default() -> Self {
        Self::Host
    }
}

/// An n-dimensional invocation matrix.
#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Matrix {
    /// If true, all invocations may execute in parallel.
    pub parallel: bool,
    /// Dimensions (outer) of cells (inner).
    pub dimensions: Vec<Vec<MatrixCell>>,
}

impl Matrix {
    /// Compile into the list of invocations (cartesian product). Each
    /// invocation's `container` field is the merge of `outer_container`
    /// (workflow+node) with every cell's container config — the task-level
    /// overlay happens later in `Compiler::plan`.
    pub fn compile(&self, outer_container: &[Option<&ContainerConfig>]) -> Result<Vec<crate::plan::Invocation>> {
        let dims_widx = self.dimensions.iter().map(|dim| {
            let mut y = 0u8;
            dim.iter()
                .map(|cell| {
                    y += 1;
                    (y - 1, cell)
                })
                .collect::<Vec<_>>()
        });

        let cp = dims_widx.multi_cartesian_product();
        let mut out = Vec::<crate::plan::Invocation>::new();
        for combo in cp {
            let cell = combo.iter().map(|(i, _)| *i).collect::<Vec<_>>();
            let mut env = HashMap::<String, String>::new();
            for (_, mc) in &combo {
                if let Some(e) = &mc.env {
                    env.extend(e.compile()?);
                }
            }

            // Container merge: outer layers first, then each cell contribution for this
            // invocation in dimension order.
            let mut layers: Vec<Option<&ContainerConfig>> = outer_container.to_vec();
            for (_, mc) in &combo {
                layers.push(mc.container.as_ref());
            }
            let container = crate::plan::merge_container(&layers)?;

            out.push(crate::plan::Invocation { env, cell, container });
        }
        Ok(out)
    }
}

/// A single matrix cell.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct MatrixCell {
    /// Optional cell-scoped env vars.
    pub env: Option<Env>,
    /// Optional cell-scoped container config. Merged with workflow/node before
    /// the owning invocation starts.
    pub container: Option<ContainerConfig>,
}

/// A single task within a node.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Task {
    /// The script body. May contain handlebars placeholders resolved at plan
    /// time.
    pub script: String,
    /// Task-level env.
    pub env: Option<Env>,
    /// Task-level shell.
    pub shell: Option<String>,
    /// Task-level workdir.
    pub workdir: Option<String>,
    /// Task-level retry policy — overrides the node's if set.
    pub retry: Option<RetryPolicy>,
    /// Task-level container config — merged over workflow/node/cell.
    pub container: Option<ContainerConfig>,
}
