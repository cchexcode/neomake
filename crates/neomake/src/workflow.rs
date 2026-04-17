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
    /// All DAG nodes.
    pub nodes: HashMap<String, Node>,
}

impl Workflow {
    /// Load and validate a workflow from a HOCON file.
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self> {
        let data = std::fs::read_to_string(&path)
            .map_err(|e| anyhow!("failed to read workflow {}: {}", path.as_ref().display(), e))?;
        Self::parse(&data)
    }

    /// Parse and validate a workflow from a HOCON string.
    pub fn parse(src: &str) -> Result<Self> {
        let wf: Workflow = hocon::HoconLoader::new()
            .load_str(src)
            .map_err(|e| anyhow!("failed to parse HOCON: {}", e))?
            .resolve()
            .map_err(|e| anyhow!("failed to deserialize workflow: {}", e))?;

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
    /// Prerequisite nodes that must execute before this one.
    pub pre: Option<Vec<NodeSelector>>,
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
    /// Compile into the list of invocations (cartesian product).
    pub fn compile(&self) -> Result<Vec<crate::plan::Invocation>> {
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
            for (_, mc) in combo {
                if let Some(e) = &mc.env {
                    env.extend(e.compile()?);
                }
            }
            out.push(crate::plan::Invocation { env, cell });
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
}
