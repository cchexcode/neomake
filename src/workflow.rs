use {
    anyhow::Result,
    itertools::Itertools,
    std::{
        collections::HashMap,
        path::Path,
    },
};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case")] // can not deny unknown fields to support YAML anchors
/// The entire workflow definition.
pub(crate) struct Workflow {
    /// The version of this workflow file (major.minor).
    pub version: String,

    /// Env vars.
    pub env: Option<Env>,

    // limiting enum ser/deser to be JSON compatible 1-entry maps (due to schema coming from schemars)
    #[serde(with = "serde_yaml::with::singleton_map_recursive")]
    #[schemars(with = "HashMap<String, Node>")]
    /// All nodes.
    pub nodes: HashMap<String, Node>,

    /// All watch nodes.
    pub watch: Option<HashMap<String, WatchExec>>,
}

impl Workflow {
    pub fn load<P: AsRef<Path>>(file: P) -> Result<Self> {
        let data = std::fs::read_to_string(&file)?;

        #[derive(Debug, serde::Deserialize)]
        struct Versioned {
            version: String,
        }
        let v = serde_yaml::from_str::<Versioned>(&data)?;

        let major_minor = env!("CARGO_PKG_VERSION").split(".").take(2).join(".");
        if &major_minor != "0.0" && &v.version != &major_minor {
            // major.minor must equal
            Err(anyhow::anyhow!(
                "workflow version {} is incompatible with this CLI version {}",
                v.version,
                env!("CARGO_PKG_VERSION")
            ))?
        }

        let wf: crate::workflow::Workflow = serde_yaml::from_str(&data)?;
        let nodes_allow_regex = fancy_regex::Regex::new(r"^[a-zA-Z0-9:_-]+$")?;
        for node in wf.nodes.keys() {
            if !nodes_allow_regex.is_match(node)? {
                Err(anyhow::anyhow!("invalid node name: {}", node))?
            }
        }
        Ok(wf)
    }
}

#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// Environment variables definitions.
pub struct Env {
    /// Regex for capturing and storing env vars during compile time.
    pub capture: Option<String>,
    /// Explicitly set env vars.
    pub vars: Option<HashMap<String, String>>,
}

impl Env {
    pub(crate) fn compile(&self) -> Result<HashMap<String, String>> {
        let mut map = self.vars.clone().or(Some(HashMap::<_, _>::new())).unwrap();
        match &self.capture {
            | Some(v) => {
                let regex = fancy_regex::Regex::new(v)?;
                let envs = std::env::vars().collect_vec();
                for e in envs {
                    if regex.is_match(&e.0)? {
                        map.insert(e.0, e.1);
                    }
                }
            },
            | None => {},
        }
        Ok(map)
    }
}

#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// A task execution environment.
pub(crate) struct Shell {
    /// The program (like "/bin/bash").
    pub program: String,
    /// Custom args (like \["-c"\]).
    pub args: Vec<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// An individual node for executing a task batch.
pub(crate) struct Node {
    /// A description of this node.
    pub description: Option<String>,
    /// Reference nodes that need to be executed prior to this one.
    pub pre: Option<Vec<String>>,

    /// An n-dimensional matrix that is executed for every item in its cartesian
    /// product.
    pub matrix: Option<Matrix>,
    /// The tasks to be executed.
    pub tasks: Vec<Task>,

    /// Env vars.
    pub env: Option<HashMap<String, String>>,
    /// Custom program to execute the scripts.
    pub shell: Option<Shell>,
    /// Custom workdir.
    pub workdir: Option<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// An entry in the n-dimensional matrix for the node execution.
pub(crate) enum Matrix {
    Dense {
        drop: Option<String>,
        dimensions: Vec<Vec<MatrixCell>>,
    },
    Sparse {
        dimensions: Vec<Vec<MatrixCell>>,
        keep: Option<String>,
    },
}

impl Matrix {
    pub(crate) fn compile(&self) -> Result<Vec<crate::plan::Invocation>> {
        let (dimensions, regex) = match self {
            | Self::Dense { drop, dimensions } => (dimensions, drop),
            | Self::Sparse { keep, dimensions } => (dimensions, keep),
        };

        let regex = match regex {
            | Some(v) => Some(fancy_regex::Regex::new(&v)?),
            | None => None,
        };

        // Bake the coords in their respective dimension into the struct itself.
        // This makes coord finding for regex (later) a breeze.
        let dims_widx = dimensions.iter().map(|d_x| {
            let mut y = 0usize;
            d_x.iter()
                .map(|d_y| {
                    y += 1;
                    (y - 1, d_y)
                })
                .collect_vec()
        });

        let cp = dims_widx.multi_cartesian_product();
        let mut v = Vec::<crate::plan::Invocation>::new();

        for next in cp {
            let coords = next.iter().map(|v| format!("{}", v.0)).join(",");

            match self {
                | Self::Dense { .. } => {
                    if let Some(regex) = &regex {
                        // drop all that match
                        if regex.is_match(&format!("{}", coords))? {
                            continue;
                        }
                    } else { // keep all
                    };
                },
                | Self::Sparse { .. } => {
                    if let Some(regex) = &regex {
                        // drop all that do not match
                        if !regex.is_match(&format!("{}", coords))? {
                            continue;
                        }
                    } else {
                        // drop all
                        continue;
                    };
                },
            }

            let mut env = HashMap::<String, String>::new();
            for m in next {
                if let Some(e) = &m.1.env {
                    env.extend(e.clone());
                }
            }

            v.push(crate::plan::Invocation { env, coords });
        }
        Ok(v)
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// An entry in the n-dimensional matrix for the node execution.
pub(crate) struct MatrixCell {
    /// Environment variables.
    pub env: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// An individual task.
pub(crate) struct Task {
    /// The script content to execute. Can contain handlebars placeholders.
    pub script: String,

    /// Explicitly set env vars.
    pub env: Option<HashMap<String, String>>,
    /// Custom program to execute the scripts.
    pub shell: Option<Shell>,
    /// Custom workdir.
    pub workdir: Option<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// Watch definition.
pub(crate) struct WatchExec {
    /// Regex filter.
    pub filter: String,
    /// Whether to process all messages or skip processing as long as one is
    /// running.
    pub queue: bool,
    /// Execution steps.
    #[serde(with = "serde_yaml::with::singleton_map_recursive")]
    #[schemars(with = "WatchExecStep")]
    pub exec: WatchExecStep,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// Single execution step.
pub(crate) enum WatchExecStep {
    /// Reference and call a node.
    Node {
        #[serde(rename = "ref")]
        ref_: String,
    },
}
