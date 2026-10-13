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
///
/// This is the source model. It is not an execution plan: optional fields,
/// includes, and guards are resolved by `crate::plan::Compiler`.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case")]
pub struct Workflow {
    /// Schema version (`major.minor`). `"0.0"` matches any CLI; otherwise it
    /// must equal the CLI's major.minor.
    pub version: String,
    /// Global environment variable configuration.
    pub env: Option<Env>,
    /// Workflow-wide default container encapsulation.
    pub container: Option<ContainerConfig>,
    /// All DAG nodes. Names are validated in `load` / `parse`.
    pub nodes: HashMap<String, Node>,
}

impl Workflow {
    /// Load and validate a workflow from a HOCON file. `include "other.conf"`
    /// directives resolve relative to the workflow file's directory.
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self> {
        let wf: Workflow = hocon::HoconLoader::new()
            .load_file(path.as_ref())
            .map_err(|e| anyhow!("failed to read workflow {}: {e}", path.as_ref().display()))?
            .resolve()
            .map_err(|e| anyhow!("failed to deserialize workflow: {e}"))?;
        Self::validate(wf)
    }

    /// Parse and validate a workflow from a HOCON string.
    ///
    /// `include` directives are disabled: the hocon crate needs a base path.
    /// Use `Workflow::load` for files with includes.
    #[cfg(test)]
    pub fn parse(src: &str) -> Result<Self> {
        let wf: Workflow = hocon::HoconLoader::new()
            .load_str(src)
            .map_err(|e| anyhow!("failed to parse HOCON: {e}"))?
            .resolve()
            .map_err(|e| anyhow!("failed to deserialize workflow: {e}"))?;
        Self::validate(wf)
    }

    fn validate(wf: Workflow) -> Result<Self> {
        let expected = env!("CARGO_PKG_VERSION").split('.').take(2).join(".");
        if !version_compatible(&wf.version, &expected) {
            return Err(anyhow!(
                "workflow version {} is incompatible with CLI {} (expected {expected} or 0.0)",
                wf.version,
                env!("CARGO_PKG_VERSION"),
            ));
        }
        for name in wf.nodes.keys() {
            if !is_valid_node_name(name) {
                return Err(anyhow!("invalid node name: {name} (expected [a-zA-Z0-9:_-]+)"));
            }
        }
        Ok(wf)
    }
}

/// `"0.0"` is the development wildcard. Any other value must equal the CLI
/// major.minor. The full package version is not accepted — the schema contract
/// is major.minor.
fn version_compatible(workflow_version: &str, cli_major_minor: &str) -> bool {
    workflow_version == "0.0" || workflow_version == cli_major_minor
}

fn is_valid_node_name(name: &str) -> bool {
    !name.is_empty()
        && name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, ':' | '_' | '-'))
}

/// Environment variable configuration: capture host env vars by regex, and/or
/// supply explicit key/value pairs.
#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Env {
    /// Regex of host env vars to capture at compile time.
    pub capture: Option<String>,
    /// Explicit env vars.
    pub vars: Option<HashMap<String, String>>,
}

impl Env {
    /// Compile into a concrete map.
    ///
    /// `host` is the process environment captured at the plan boundary.
    /// Explicit `vars` are applied first; captured host vars overlay them.
    /// This does not read `std::env`.
    pub fn compile(&self, host: &HashMap<String, String>) -> Result<HashMap<String, String>> {
        let mut map = self.vars.clone().unwrap_or_default();
        if let Some(regex) = &self.capture {
            let re = fancy_regex::Regex::new(regex).map_err(|e| anyhow!("invalid env capture regex `{regex}`: {e}"))?;
            for (k, v) in host {
                if re
                    .is_match(k)
                    .map_err(|e| anyhow!("env capture match failed for `{k}`: {e}"))?
                {
                    map.insert(k.clone(), v.clone());
                }
            }
        }
        Ok(map)
    }
}

/// A node selector. Either `{ name = "..." }` or `{ regex = "..." }` in HOCON.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(untagged, deny_unknown_fields)]
pub enum NodeSelector {
    Name { name: String },
    Regex { regex: String },
}

impl NodeSelector {
    /// Matched node names. A `name` selector returns that name even when it is
    /// absent from `candidates`; callers decide whether absence is an error.
    pub fn resolve<'a, I>(&self, candidates: I) -> Result<Vec<String>>
    where I: IntoIterator<Item=&'a String> {
        match self {
            | NodeSelector::Name { name } => Ok(vec![name.clone()]),
            | NodeSelector::Regex { regex } => {
                let re = fancy_regex::Regex::new(regex).map_err(|e| anyhow!("invalid node regex `{regex}`: {e}"))?;
                let mut out = Vec::<String>::new();
                for k in candidates {
                    if re
                        .is_match(k)
                        .map_err(|e| anyhow!("node regex match failed for `{k}`: {e}"))?
                    {
                        out.push(k.clone());
                    }
                }
                Ok(out)
            },
        }
    }
}

/// A single DAG node.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Node {
    /// Optional description shown in `list`.
    pub description: Option<String>,
    /// Prerequisite nodes that must execute before this one (ordering only).
    pub pre: Option<Vec<NodeSelector>>,
    /// Input producers whose stdout is streamed to this node's stdin.
    ///
    /// Each producer is also an implicit `pre` dependency. A producer skipped
    /// by its `when` guard is a plan error — there is no stdout to stream.
    pub inputs: Option<Vec<NodeSelector>>,
    /// Optional conditional guard, evaluated at plan time. A falsy guard omits
    /// this node. Dependents that only `pre` it still run; dependents that
    /// `inputs` it fail the plan.
    pub when: Option<WhenCondition>,
    /// Node-level retry policy. A task-level policy overrides this.
    pub retry: Option<RetryPolicy>,
    /// Optional n-dimensional invocation matrix.
    pub matrix: Option<Matrix>,
    /// Tasks to execute, in order, for every invocation.
    pub tasks: Vec<Task>,
    /// Node-level env.
    pub env: Option<Env>,
    /// Node-level shell (for example `/bin/bash -c`).
    pub shell: Option<String>,
    /// Node-level workdir.
    pub workdir: Option<String>,
    /// Node-level container config. Merged over the workflow default.
    pub container: Option<ContainerConfig>,
}

/// Conditional guard on a node.
///
/// - `when = "{{ args.env }}"` — handlebars template; a truthy result includes
///   the node. Missing args are falsy (non-strict render).
/// - `when { script = "...", shell = "..." }` — run at plan time; exit 0
///   includes.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(untagged, deny_unknown_fields)]
pub enum WhenCondition {
    Template(String),
    Script {
        /// Shell script. Exit code 0 means include this node.
        script: String,
        /// Optional shell override. Defaults to `/bin/sh -c`.
        #[serde(default)]
        shell: Option<String>,
    },
}

/// Retry policy. `attempts` includes the first try.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct RetryPolicy {
    /// Total attempts, including the first try. `1` disables retrying.
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

impl RetryPolicy {
    /// Delay before the next try. `attempt` is the attempt that just failed,
    /// starting at 1.
    pub fn delay_after(&self, attempt: u32) -> std::time::Duration {
        let base = self.delay_ms;
        let ms = match self.backoff {
            | Backoff::Fixed => base,
            | Backoff::Linear => base.saturating_mul(u64::from(attempt)),
            | Backoff::Exponential => {
                let shift = attempt.saturating_sub(1).min(16);
                base.saturating_mul(1u64 << shift)
            },
        };
        std::time::Duration::from_millis(ms)
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum Backoff {
    /// `delay_ms` between every retry.
    #[default]
    Fixed,
    /// `delay_ms * attempt` — 1x, 2x, 3x, …
    Linear,
    /// `delay_ms * 2^(attempt-1)` — 1x, 2x, 4x, … capped at 2^16.
    Exponential,
}

/// Optional container encapsulation. May be declared at workflow, node, matrix
/// cell, or task scope and is merged innermost-wins by the compiler. A task
/// runs containerized iff the merged result has `image` set and
/// `disabled != true`.
#[derive(Debug, Clone, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct ContainerConfig {
    /// OCI image reference, for example `alpine:latest`.
    pub image: Option<String>,
    /// Working directory inside the container. Defaults to `/work` when
    /// resolved.
    pub workdir: Option<String>,
    /// User mapping. See `ContainerUser`. Defaults to `host`.
    pub user: Option<ContainerUser>,
    /// Bind-mount the host CWD at `workdir`. Defaults to true when resolved.
    pub mount: Option<bool>,
    /// Opt out at this scope. `disabled = true` forces a host execution even if
    /// an outer scope set an image. `disabled = false` re-enables an outer
    /// opt-out.
    pub disabled: Option<bool>,
    /// Runtime selection. `{ docker { … } }` or `{ podman { … } }`, carrying
    /// that runtime's extras. Defaults to docker with empty extras when
    /// resolved.
    pub runtime: Option<ContainerRuntime>,
}

/// Runtime plus its own extras. Untagged so HOCON is `runtime { docker { … }
/// }`.
///
/// `extra_args` accumulate across merge levels only while consecutive levels
/// keep the same variant. Switching variants discards the previous extras.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
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
    pub fn extras(&self) -> &RuntimeExtras {
        match self {
            | ContainerRuntime::Docker { docker } => docker,
            | ContainerRuntime::Podman { podman } => podman,
        }
    }
}

/// Runtime-specific knobs. Currently just `extra_args`.
#[derive(Debug, Clone, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct RuntimeExtras {
    /// Extra args appended to `<runtime> run`, after neomake's own flags.
    #[serde(default)]
    pub extra_args: Vec<String>,
}

/// User mapping for the container process.
///
/// Wire form is a string: `host`, `root`, or a literal passed to `--user`
/// (`1000:1000`, `appuser`). `host` and `root` are reserved.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum ContainerUser {
    /// `--user <host-uid>:<host-gid>`. Default.
    #[default]
    Host,
    /// No `--user` flag.
    Root,
    /// Passed verbatim to `--user`.
    Explicit(String),
}

impl ContainerUser {
    fn as_str(&self) -> &str {
        match self {
            | Self::Host => "host",
            | Self::Root => "root",
            | Self::Explicit(s) => s,
        }
    }

    fn parse(s: &str) -> std::result::Result<Self, String> {
        if s.is_empty() {
            return Err("container user must not be empty".to_owned());
        }
        Ok(match s {
            | "host" => Self::Host,
            | "root" => Self::Root,
            | other => Self::Explicit(other.to_owned()),
        })
    }
}

impl serde::Serialize for ContainerUser {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> serde::Deserialize<'de> for ContainerUser {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        Self::parse(&s).map_err(serde::de::Error::custom)
    }
}

impl schemars::JsonSchema for ContainerUser {
    fn schema_name() -> String {
        "ContainerUser".to_owned()
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        <String as schemars::JsonSchema>::json_schema(gen)
    }

    fn is_referenceable() -> bool {
        false
    }
}

/// An n-dimensional invocation matrix.
#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Matrix {
    /// If true, each invocation may execute as its own batch.
    pub parallel: bool,
    /// Dimensions (outer) of cells (inner).
    pub dimensions: Vec<Vec<MatrixCell>>,
}

/// One cartesian cell: coordinates plus the cells that produced them.
#[derive(Debug)]
pub struct MatrixPoint<'a> {
    pub cell: Vec<u32>,
    pub parts: Vec<&'a MatrixCell>,
}

impl Matrix {
    /// Cartesian product of the dimensions.
    ///
    /// An empty dimension list is one invocation with no coordinates (same as
    /// no matrix). An empty dimension is an error — it would silently
    /// produce zero invocations. Indices are `u32` and do not wrap.
    pub fn expand(&self) -> Result<Vec<MatrixPoint<'_>>> {
        for (i, dim) in self.dimensions.iter().enumerate() {
            if dim.is_empty() {
                return Err(anyhow!("matrix dimension {i} is empty"));
            }
        }
        if self.dimensions.is_empty() {
            return Ok(vec![MatrixPoint {
                cell: vec![],
                parts: vec![],
            }]);
        }

        let dims = self
            .dimensions
            .iter()
            .map(|dim| {
                dim.iter()
                    .enumerate()
                    .map(|(i, cell)| {
                        let idx = u32::try_from(i).map_err(|_| anyhow!("matrix dimension has too many cells"))?;
                        Ok((idx, cell))
                    })
                    .collect::<Result<Vec<_>>>()
            })
            .collect::<Result<Vec<_>>>()?;

        let mut out = Vec::new();
        for combo in dims.into_iter().multi_cartesian_product() {
            let mut cell = Vec::with_capacity(combo.len());
            let mut parts = Vec::with_capacity(combo.len());
            for (idx, part) in combo {
                cell.push(idx);
                parts.push(part);
            }
            out.push(MatrixPoint { cell, parts });
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
    /// Optional cell-scoped container config.
    pub container: Option<ContainerConfig>,
}

/// A single task within a node. Fields overlay the node, which overlays the
/// workflow.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub struct Task {
    /// Script body. Handlebars placeholders are resolved at plan time.
    pub script: String,
    /// Task-level env.
    pub env: Option<Env>,
    /// Task-level shell.
    pub shell: Option<String>,
    /// Task-level workdir. On the host this is the process cwd; in a container
    /// it is the `-w` argument.
    pub workdir: Option<String>,
    /// Task-level retry policy. Overrides the node's if set.
    pub retry: Option<RetryPolicy>,
    /// Task-level container config. Merged over workflow, node, and cell.
    pub container: Option<ContainerConfig>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn retry_delay_matches_backoff() {
        let fixed = RetryPolicy {
            attempts: 3,
            backoff: Backoff::Fixed,
            delay_ms: 100,
        };
        assert_eq!(fixed.delay_after(1).as_millis(), 100);
        assert_eq!(fixed.delay_after(3).as_millis(), 100);

        let linear = RetryPolicy {
            attempts: 3,
            backoff: Backoff::Linear,
            delay_ms: 100,
        };
        assert_eq!(linear.delay_after(2).as_millis(), 200);

        let exponential = RetryPolicy {
            attempts: 20,
            backoff: Backoff::Exponential,
            delay_ms: 10,
        };
        assert_eq!(exponential.delay_after(1).as_millis(), 10);
        assert_eq!(exponential.delay_after(3).as_millis(), 40);
        assert_eq!(exponential.delay_after(20).as_millis(), 10 * (1u128 << 16));
    }

    #[test]
    fn version_wildcard_and_mismatch() {
        assert!(version_compatible("0.0", "1.2"));
        assert!(version_compatible("1.2", "1.2"));
        assert!(!version_compatible("1.2", "1.3"));
        assert!(!version_compatible("1.2.0", "1.2"));
        assert!(!version_compatible("9.9", "0.0"));
    }

    #[test]
    fn parse_rejects_incompatible_version_and_bad_names() {
        assert!(Workflow::parse("version = \"9.9\"\nnodes { a { tasks = [] } }").is_err());
        assert!(Workflow::parse("version = \"0.0\"\nnodes { \"bad name\" { tasks = [] } }").is_err());
        assert!(Workflow::parse("version = \"0.0\"\nnodes { ok { tasks = [] } }").is_ok());
    }

    #[test]
    fn env_capture_uses_the_injected_host_only() {
        let env = Env {
            capture: Some("^CAPTURED$".to_owned()),
            vars: Some(HashMap::from([("EXPLICIT".to_owned(), "1".to_owned())])),
        };
        let host = HashMap::from([
            ("CAPTURED".to_owned(), "yes".to_owned()),
            ("PATH".to_owned(), "/bin".to_owned()),
        ]);
        let compiled = env.compile(&host).unwrap();
        assert_eq!(compiled.get("CAPTURED").map(String::as_str), Some("yes"));
        assert_eq!(compiled.get("EXPLICIT").map(String::as_str), Some("1"));
        assert!(!compiled.contains_key("PATH"));
    }

    #[test]
    fn capture_overlays_explicit_vars() {
        let env = Env {
            capture: Some("^K$".to_owned()),
            vars: Some(HashMap::from([("K".to_owned(), "explicit".to_owned())])),
        };
        let host = HashMap::from([("K".to_owned(), "host".to_owned())]);
        assert_eq!(env.compile(&host).unwrap().get("K").map(String::as_str), Some("host"));
    }

    #[test]
    fn container_user_wire_form() {
        let host: ContainerUser = serde_json::from_str("\"host\"").unwrap();
        let root: ContainerUser = serde_json::from_str("\"root\"").unwrap();
        let explicit: ContainerUser = serde_json::from_str("\"1000:1000\"").unwrap();
        assert_eq!(host, ContainerUser::Host);
        assert_eq!(root, ContainerUser::Root);
        assert_eq!(explicit, ContainerUser::Explicit("1000:1000".to_owned()));
        assert_eq!(serde_json::to_string(&explicit).unwrap(), "\"1000:1000\"");
        assert!(serde_json::from_str::<ContainerUser>("\"\"").is_err());
    }

    #[test]
    fn matrix_expand_is_cartesian_and_does_not_wrap() {
        let cell = |label: &str| {
            MatrixCell {
                env: Some(Env {
                    capture: None,
                    vars: Some(HashMap::from([("L".to_owned(), label.to_owned())])),
                }),
                container: None,
            }
        };
        let matrix = Matrix {
            parallel: true,
            dimensions: vec![vec![cell("a0"), cell("a1")], vec![cell("b0"), cell("b1")]],
        };
        let points = matrix.expand().unwrap();
        let coords: Vec<_> = points.iter().map(|p| p.cell.clone()).collect();
        assert_eq!(coords, vec![vec![0, 0], vec![0, 1], vec![1, 0], vec![1, 1]]);

        let wide = Matrix {
            parallel: false,
            dimensions: vec![vec![
                MatrixCell {
                    env: None,
                    container: None,
                };
                300
            ]],
        };
        let points = wide.expand().unwrap();
        assert_eq!(points.len(), 300);
        assert_eq!(points[299].cell, vec![299]);
    }

    #[test]
    fn matrix_empty_dimension_errors_and_zero_dimensions_is_one_point() {
        let empty_dim = Matrix {
            parallel: false,
            dimensions: vec![vec![]],
        };
        assert!(empty_dim.expand().is_err());

        let no_dims = Matrix {
            parallel: true,
            dimensions: vec![],
        };
        let points = no_dims.expand().unwrap();
        assert_eq!(points.len(), 1);
        assert!(points[0].cell.is_empty());
    }
}
