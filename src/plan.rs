use std::collections::HashMap;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(crate) struct ExecutionPlan {
    pub version: String,
    pub nodes: HashMap<String, Node>,
    pub stages: Vec<Stage>,
    pub env: HashMap<String, String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(crate) struct Stage {
    pub nodes: Vec<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(crate) struct Node {
    pub parallel: bool,
    pub invocations: Vec<Invocation>,
    pub tasks: Vec<Task>,

    pub env: HashMap<String, String>,
    pub shell: Option<String>,
    pub workdir: Option<String>,
}

#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(crate) struct Invocation {
    pub cell: Vec<u8>,
    pub env: HashMap<String, String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(crate) struct Task {
    pub cmd: String,

    pub env: HashMap<String, String>,
    pub shell: Option<String>,
    pub workdir: Option<String>,
}
