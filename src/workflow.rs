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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum NodeSelector {
    Name(String),
    Regex(String),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// An individual node for executing a task batch.
pub(crate) struct Node {
    /// A description of this node.
    pub description: Option<String>,
    /// Reference nodes that need to be executed prior to this one.
    pub pre: Option<Vec<NodeSelector>>,

    /// An n-dimensional matrix that is executed for every item in its cartesian
    /// product.
    pub matrix: Option<Matrix>,
    /// The tasks to be executed.
    pub tasks: Vec<Task>,

    /// Env vars.
    pub env: Option<Env>,
    /// Custom program to execute the scripts.
    pub shell: Option<String>,
    /// Custom workdir.
    pub workdir: Option<String>,
}

#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// An entry in the n-dimensional matrix for the node execution.
pub(crate) struct Matrix {
    pub parallel: bool,
    pub dimensions: Vec<Vec<MatrixCell>>,
}

impl Matrix {
    pub(crate) fn compile(&self) -> Result<Vec<crate::plan::Invocation>> {
        // Bake the coords in their respective dimension into the struct itself.
        // This makes coord finding for regex (later) a breeze.
        let dims_widx = self.dimensions.iter().map(|d_x| {
            let mut y = 0_u8;
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
            let cell = next.iter().map(|v| v.0).collect::<Vec<_>>();

            let mut env = HashMap::<String, String>::new();
            for m in next {
                if let Some(e) = &m.1.env {
                    env.extend(e.compile()?);
                }
            }

            v.push(crate::plan::Invocation { env, cell });
        }
        Ok(v)
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// An entry in the n-dimensional matrix for the node execution.
pub(crate) struct MatrixCell {
    /// Environment variables.
    pub env: Option<Env>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
/// An individual task.
pub(crate) struct Task {
    /// The script content to execute. Can contain handlebars placeholders.
    pub script: String,

    /// Explicitly set env vars.
    pub env: Option<Env>,
    /// Custom program to execute the scripts.
    pub shell: Option<String>,
    /// Custom workdir.
    pub workdir: Option<String>,
}
