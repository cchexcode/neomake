use {
    crate::{
        plan::ExecutionPlan,
        workflow::Workflow,
    },
    anyhow::Result,
    clap::{
        Arg,
        ArgAction,
    },
    itertools::Itertools,
    std::{
        collections::{
            HashMap,
            HashSet,
        },
        io::Read,
        iter::FromIterator,
        str::FromStr,
    },
};

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Privilege {
    Normal,
    Experimental,
}

#[derive(Debug)]
pub(crate) struct CallArgs {
    pub privileges: Privilege,
    pub command: Command,
}

impl CallArgs {
    pub(crate) fn validate(&self) -> Result<()> {
        if self.privileges == Privilege::Experimental {
            return Ok(());
        }

        match &self.command {
            | Command::Multiplex { .. } => Err(anyhow::anyhow!("experimental command: multiplex"))?,
            | Command::Watch { .. } => Err(anyhow::anyhow!("experimental command: watch"))?,
            | Command::List { format, .. } => {
                match &format {
                    | CommandListFormat::Custom => Err(anyhow::anyhow!("experimental format: custom"))?,
                    | _ => {},
                }
            },
            | _ => (),
        }

        Ok(())
    }
}

#[derive(Debug)]
pub(crate) enum ManualFormat {
    Manpages,
    Markdown,
}

#[derive(Debug)]
pub(crate) enum Format {
    YAML,
    #[cfg(feature = "format+json")]
    JSON {
        pretty: bool,
    },
    #[cfg(feature = "format+ron")]
    RON {
        pretty: bool,
    },
}

#[derive(Debug)]
pub(crate) enum CommandListFormat {
    Standard(Format),
    Custom,
}

impl Format {
    pub(crate) fn serialize<T: serde::Serialize>(&self, arg: &T) -> Result<String> {
        match self {
            | crate::args::Format::YAML => Ok(serde_yaml::to_string(arg)?),
            #[cfg(feature = "format+json")]
            | crate::args::Format::JSON { pretty } => {
                if *pretty {
                    Ok(serde_json::to_string_pretty(arg)?)
                } else {
                    Ok(serde_json::to_string(arg)?)
                }
            },
            #[cfg(feature = "format+ron")]
            | crate::args::Format::RON { pretty } => {
                if *pretty {
                    Ok(ron::ser::to_string_pretty(
                        arg,
                        ron::ser::PrettyConfig::new()
                            .compact_arrays(true)
                            .enumerate_arrays(false)
                            .new_line("\n".to_owned()), // no windows on my turf
                    )?)
                } else {
                    Ok(ron::ser::to_string(arg)?)
                }
            },
        }
    }

    pub(crate) fn deserialize<T: serde::de::DeserializeOwned>(&self, s: &str) -> Result<T> {
        match self {
            | crate::args::Format::YAML => Ok(serde_yaml::from_str::<T>(s)?),
            #[cfg(feature = "format+json")]
            | crate::args::Format::JSON { .. } => Ok(serde_json::from_str::<T>(s)?),
            #[cfg(feature = "format+ron")]
            | crate::args::Format::RON { .. } => Ok(ron::from_str::<T>(s)?),
        }
    }

    fn from_arg(arg: &str) -> Result<Self> {
        match arg {
            | "yaml" => Ok(Format::YAML),
            #[cfg(feature = "format+json")]
            | "json" => Ok(Format::JSON { pretty: false }),
            #[cfg(feature = "format+json")]
            | "json+p" => Ok(Format::JSON { pretty: true }),
            #[cfg(feature = "format+ron")]
            | "ron" => Ok(Format::RON { pretty: false }),
            #[cfg(feature = "format+ron")]
            | "ron+p" => Ok(Format::RON { pretty: true }),
            | _ => Err(anyhow::anyhow!("unknown format")),
        }
    }
}

#[derive(Debug)]
pub(crate) enum WorkflowInitTemplate {
    Min,
    Max,
    Python,
}

impl WorkflowInitTemplate {
    pub(crate) fn render(&self) -> String {
        match self {
            | WorkflowInitTemplate::Min => include_str!("../res/templates/min.neomake.yaml").to_owned(),
            | WorkflowInitTemplate::Max => include_str!("../res/templates/max.neomake.yaml").to_owned(),
            | WorkflowInitTemplate::Python => include_str!("../res/templates/python.neomake.yaml").to_owned(),
        }
    }
}

#[derive(Debug)]
pub(crate) enum WorkflowInitOutput {
    Stdout,
    File(String),
}

#[derive(Debug)]
pub(crate) enum Nodes {
    Arr(HashSet<String>),
    Regex(String),
}

impl Nodes {
    pub(crate) fn select(self, wf: &Workflow) -> Result<HashSet<String>> {
        match self {
            | Self::Arr(v) => Ok(v),
            | Self::Regex(v) => {
                let re = fancy_regex::Regex::new(&v)?;
                let mut hs = HashSet::<String>::new();
                for node in wf.nodes.keys() {
                    if re.is_match(&node)? {
                        hs.insert(node.clone());
                    }
                }
                Ok(hs)
            },
        }
    }
}

#[derive(Debug)]
pub(crate) enum WorkflowCommand {
    Schema,
    Init {
        template: WorkflowInitTemplate,
        output: WorkflowInitOutput,
    },
}

#[derive(Debug)]
pub(crate) enum Command {
    Manual {
        path: String,
        format: ManualFormat,
    },
    Autocomplete {
        path: String,
        shell: clap_complete::Shell,
    },
    Workflow(WorkflowCommand),
    Execute {
        plan: ExecutionPlan,
        workers: usize,
        no_stdout: bool,
        no_stderr: bool,
    },
    Plan {
        workflow: String,
        nodes: Nodes,
        args: HashMap<String, String>,
        format: Format,
    },
    List {
        workflow: String,
        format: CommandListFormat,
    },
    Describe {
        workflow: String,
        nodes: Nodes,
        format: Format,
    },
    Watch {
        filter: String,
        root: String,
        program: Vec<String>,
        stderr: usize,
        commands: Vec<String>,
        parallelism: Option<usize>,
    },
    Multiplex {
        program: Vec<String>,
        stdout: Option<Format>,
        stderr: usize,
        commands: Vec<String>,
        parallelism: Option<usize>,
    },
}

pub(crate) struct ClapArgumentLoader {}

impl ClapArgumentLoader {
    pub(crate) fn root_command() -> clap::Command {
        #[allow(unused_mut)] // features will add
        let mut output_formats = vec!["yaml"];
        #[cfg(feature = "format+json")]
        output_formats.extend(["json", "json+p"]);
        #[cfg(feature = "format+ron")]
        output_formats.extend(["ron", "ron+p"]);
        // strip format modifiers ("+\w")
        let input_formats = output_formats.iter().filter(|v| !v.ends_with("+p")).collect_vec();
        assert!(output_formats.len() > 0);

        clap::Command::new("neomake")
            .version(env!("CARGO_PKG_VERSION"))
            .about("A rusty makefile alternative / task runner.")
            .author("cchexcode <alexanderh.weber@outlook.com>")
            .propagate_version(true)
            .subcommand_required(true)
            .args([Arg::new("experimental")
                .short('e')
                .long("experimental")
                .help("Enables experimental features.")
                .num_args(0)])
            .subcommand(
                clap::Command::new("man")
                    .about("Renders the manual.")
                    .arg(clap::Arg::new("out").short('o').long("out").required(true))
                    .arg(
                        clap::Arg::new("format")
                            .short('f')
                            .long("format")
                            .value_parser(["manpages", "markdown"])
                            .required(true),
                    ),
            )
            .subcommand(
                clap::Command::new("autocomplete")
                    .about("Renders shell completion scripts.")
                    .arg(clap::Arg::new("out").short('o').long("out").required(true))
                    .arg(
                        clap::Arg::new("shell")
                            .short('s')
                            .long("shell")
                            .value_parser(["bash", "zsh", "fish", "elvish", "powershell"])
                            .required(true),
                    ),
            )
            .subcommand(
                clap::Command::new("workflow")
                    .about("Workflow related subcommands.")
                    .subcommand(
                        clap::Command::new("init")
                            .about("Initializes a new template workflow.")
                            .arg(
                                Arg::new("template")
                                    .short('t')
                                    .long("template")
                                    .help("The template to init with.")
                                    .default_value("min")
                                    .value_parser(["min", "max", "python"]),
                            )
                            .arg(
                                Arg::new("output")
                                    .short('o')
                                    .long("output")
                                    .help("The file to render the output to. \"-\" renders to STDOUT.")
                                    .default_value("./neomake.yaml"),
                            ),
                    )
                    .subcommand(clap::Command::new("schema").about("Renders the workflow schema to STDOUT.")),
            )
            .subcommand(
                clap::Command::new("plan")
                    .about("Creates an execution plan.")
                    .visible_aliases(&["p"])
                    .arg(
                        Arg::new("workflow")
                            .long("workflow")
                            .help("The workflow file to use.")
                            .default_value("./neomake.yaml"),
                    )
                    .arg(
                        Arg::new("node")
                            .short('n')
                            .long("node")
                            .action(ArgAction::Append)
                            .conflicts_with("regex")
                            .required_unless_present("regex")
                            .help("Adding a node to the plan."),
                    )
                    .arg(
                        Arg::new("regex")
                            .short('r')
                            .long("regex")
                            .conflicts_with("node")
                            .required_unless_present("node")
                            .help("Adding a node to the plan."),
                    )
                    .arg(
                        Arg::new("arg")
                            .short('a')
                            .long("arg")
                            .action(ArgAction::Append)
                            .help("Specifies a value for handlebars placeholders."),
                    )
                    .arg(
                        Arg::new("output")
                            .short('o')
                            .long("output")
                            .help("Specifies the output format.")
                            .value_parser(output_formats.clone())
                            .default_value(output_formats.first().unwrap()),
                    ),
            )
            .subcommand(
                clap::Command::new("execute")
                    .about("Executes an execution plan.")
                    .visible_aliases(&["exec", "x"])
                    .arg(
                        Arg::new("format")
                            .short('f')
                            .long("format")
                            .help("The format of the execution plan.")
                            .value_parser(input_formats.clone())
                            .default_value(*input_formats.first().unwrap()),
                    )
                    .arg(
                        Arg::new("workers")
                            .short('w')
                            .long("workers")
                            .help("Defines how many worker threads are created in the OS thread pool.")
                            .default_value("1"),
                    )
                    .arg(
                        Arg::new("no-stdout")
                            .long("no-stdout")
                            .help(
                                "Disables any output to STDOUT. Useful for preventing leakage of secrets and keeping \
                                 the logs clean.",
                            )
                            .num_args(0),
                    )
                    .arg(
                        Arg::new("no-stderr")
                            .long("no-stderr")
                            .help(
                                "Disables any output to STDERR. Useful for preventing leakage of secrets and keeping \
                                 the logs clean.",
                            )
                            .num_args(0),
                    ),
            )
            .subcommand(
                clap::Command::new("describe")
                    .about("Describes which nodes are executed in which stages.")
                    .visible_aliases(&["desc", "d"])
                    .arg(
                        Arg::new("workflow")
                            .long("workflow")
                            .help("The workflow file to use.")
                            .default_value("./neomake.yaml"),
                    )
                    .arg(
                        Arg::new("node")
                            .short('n')
                            .long("node")
                            .action(ArgAction::Append)
                            .conflicts_with("regex")
                            .required_unless_present("regex")
                            .help("Adding a node."),
                    )
                    .arg(
                        Arg::new("regex")
                            .short('r')
                            .long("regex")
                            .conflicts_with("node")
                            .required_unless_present("node")
                            .help("Adding a node to the plan."),
                    )
                    .arg(
                        Arg::new("output")
                            .short('o')
                            .long("output")
                            .help("The output format.")
                            .value_parser(output_formats.clone())
                            .default_value(output_formats.first().unwrap()),
                    ),
            )
            .subcommand(
                clap::Command::new("list")
                    .about("Lists all available nodes.")
                    .visible_aliases(&["ls", "l"])
                    .arg(
                        clap::Arg::new("workflow")
                            .long("workflow")
                            .help("The workflow file to use.")
                            .default_value("./neomake.yaml"),
                    )
                    .arg(
                        Arg::new("output")
                            .short('o')
                            .long("output")
                            .help("The output format.")
                            .value_parser(
                                output_formats
                                    .clone()
                                    .into_iter()
                                    .chain(vec!["custom"])
                                    .collect::<Vec<_>>(),
                            )
                            .default_value(output_formats.first().unwrap()),
                    ),
            )
            .subcommand(
                clap::Command::new("multiplex")
                    .about("Multiplex commands")
                    .visible_aliases(&["m", "mp"])
                    .args([
                        clap::Arg::new("program")
                            .long("program")
                            .help("Defines the program used to execute the commands given.")
                            .default_value("/bin/sh -c"),
                        clap::Arg::new("stderr")
                            .long("stderr")
                            .help("Defines the length of stderr to display.")
                            .default_value("3"),
                        clap::Arg::new("stdout")
                            .long("stdout")
                            .help(
                                "Marks whether the stdout of the processes are captured and returned in a structured \
                                 format to stdout.",
                            )
                            .value_parser(output_formats.clone()),
                        clap::Arg::new("parallelism")
                            .long("parallelism")
                            .short('p')
                            .help("Set the maximum amount of (sub) processes that run in parallel."),
                        clap::Arg::new("command")
                            .short('c')
                            .long("command")
                            .help("A command to be executed.")
                            .action(ArgAction::Append),
                    ]),
            )
            .subcommand(
                clap::Command::new("watch").about("Execute watch.").args([
                    Arg::new("filter").short('f').long("filter").required(true),
                    Arg::new("root").short('r').long("root").default_value("./"),
                    Arg::new("program")
                        .long("program")
                        .help("Defines the program used to execute the commands given.")
                        .default_value("/bin/sh -c"),
                    Arg::new("parallelism")
                        .short('p')
                        .long("parallelism")
                        .help("Defines how many worker threads are created in the OS thread pool.")
                        .default_value("1"),
                    Arg::new("stderr")
                        .long("stderr")
                        .help("Defines the length of stderr to display.")
                        .default_value("8"),
                    Arg::new("command")
                        .short('c')
                        .long("command")
                        .help("A command to be executed.")
                        .action(ArgAction::Append),
                ]),
            )
    }

    pub(crate) fn load() -> Result<CallArgs> {
        let command = Self::root_command().get_matches();

        let privileges = if command.get_flag("experimental") {
            Privilege::Experimental
        } else {
            Privilege::Normal
        };

        fn parse_nodes(x: &clap::ArgMatches) -> Nodes {
            match x.get_many::<String>("node") {
                | Some(v) => Nodes::Arr(HashSet::<String>::from_iter(v.into_iter().map(|v| v.to_owned()))),
                | None => Nodes::Regex(x.get_one::<String>("regex").unwrap().to_owned()),
            }
        }

        let cmd = if let Some(subc) = command.subcommand_matches("man") {
            Command::Manual {
                path: subc.get_one::<String>("out").unwrap().into(),
                format: match subc.get_one::<String>("format").unwrap().as_str() {
                    | "manpages" => ManualFormat::Manpages,
                    | "markdown" => ManualFormat::Markdown,
                    | _ => return Err(anyhow::anyhow!("argument \"format\": unknown format")),
                },
            }
        } else if let Some(subc) = command.subcommand_matches("autocomplete") {
            Command::Autocomplete {
                path: subc.get_one::<String>("out").unwrap().into(),
                shell: clap_complete::Shell::from_str(subc.get_one::<String>("shell").unwrap().as_str()).unwrap(),
            }
        } else if let Some(x) = command.subcommand_matches("workflow") {
            if let Some(x) = x.subcommand_matches("init") {
                Command::Workflow(WorkflowCommand::Init {
                    template: match x.get_one::<String>("template").unwrap().as_str() {
                        | "min" => WorkflowInitTemplate::Min,
                        | "max" => WorkflowInitTemplate::Max,
                        | "python" => WorkflowInitTemplate::Python,
                        | _ => return Err(anyhow::anyhow!("argument \"template\": unknown template")),
                    },
                    output: match x.get_one::<String>("output").unwrap().as_str() {
                        | "-" => WorkflowInitOutput::Stdout,
                        | s => WorkflowInitOutput::File(s.to_owned()),
                    },
                })
            } else if let Some(_) = x.subcommand_matches("schema") {
                Command::Workflow(WorkflowCommand::Schema)
            } else {
                return Err(anyhow::anyhow!("unknown command"));
            }
        } else if let Some(x) = command.subcommand_matches("execute") {
            let mut args_map: HashMap<String, String> = HashMap::new();
            if let Some(args) = x.get_many::<String>("arg") {
                for v_arg in args {
                    let spl: Vec<&str> = v_arg.splitn(2, "=").collect();
                    args_map.insert(spl[0].to_owned(), spl[1].to_owned());
                }
            }

            let format = Format::from_arg(x.get_one::<String>("format").unwrap().as_str())?;
            let mut plan = String::new();
            std::io::stdin().read_to_string(&mut plan)?;

            Command::Execute {
                plan: format.deserialize::<ExecutionPlan>(&plan)?,
                workers: str::parse::<usize>(x.get_one::<String>("workers").unwrap()).unwrap(),
                no_stdout: x.get_flag("no-stdout"),
                no_stderr: x.get_flag("no-stderr"),
            }
        } else if let Some(x) = command.subcommand_matches("plan") {
            let mut args_map: HashMap<String, String> = HashMap::new();
            if let Some(args) = x.get_many::<String>("arg") {
                for v_arg in args {
                    let spl: Vec<&str> = v_arg.splitn(2, "=").collect();
                    args_map.insert(spl[0].to_owned(), spl[1].to_owned());
                }
            }

            Command::Plan {
                workflow: x.get_one::<String>("workflow").unwrap().clone(),
                nodes: parse_nodes(x),
                args: args_map,
                format: Format::from_arg(x.get_one::<String>("output").unwrap().as_str())?,
            }
        } else if let Some(x) = command.subcommand_matches("list") {
            Command::List {
                workflow: x.get_one::<String>("workflow").unwrap().clone(),
                format: match x.get_one::<String>("output").unwrap().as_str() {
                    | "custom" => CommandListFormat::Custom,
                    | v => CommandListFormat::Standard(Format::from_arg(v)?),
                },
            }
        } else if let Some(x) = command.subcommand_matches("describe") {
            Command::Describe {
                workflow: x.get_one::<String>("workflow").unwrap().clone(),
                nodes: parse_nodes(x),
                format: Format::from_arg(x.get_one::<String>("output").unwrap().as_str())?,
            }
        } else if let Some(x) = command.subcommand_matches("watch") {
            let commands = x
                .get_many::<String>("command")
                .unwrap_or_default()
                .cloned()
                .collect_vec();

            let program = x
                .get_one::<String>("program")
                .unwrap()
                .split_whitespace()
                .into_iter()
                .map(|v| v.to_owned())
                .collect::<Vec<_>>();
            Command::Watch {
                filter: x.get_one::<String>("filter").unwrap().to_owned(),
                root: x.get_one::<String>("root").unwrap().to_owned(),
                program,
                stderr: x.get_one::<String>("stderr").unwrap().parse::<usize>()?,
                commands,
                parallelism: match x.get_one::<String>("parallelism") {
                    | Some(v) => Some(v.parse::<usize>().unwrap()),
                    | None => None,
                },
            }
        } else if let Some(x) = command.subcommand_matches("multiplex") {
            let commands = x
                .get_many::<String>("command")
                .unwrap_or_default()
                .cloned()
                .collect_vec();

            let program = x
                .get_one::<String>("program")
                .unwrap()
                .split_whitespace()
                .into_iter()
                .map(|v| v.to_owned())
                .collect::<Vec<_>>();
            Command::Multiplex {
                program,
                stderr: x.get_one::<String>("stderr").unwrap().parse::<usize>()?,
                stdout: match x.get_one::<String>("stdout") {
                    | Some(v) => Some(Format::from_arg(v)?),
                    | None => None,
                },
                commands,
                parallelism: match x.get_one::<String>("parallelism") {
                    | Some(v) => Some(v.parse::<usize>().unwrap()),
                    | None => None,
                },
            }
        } else {
            return Err(anyhow::anyhow!("unknown command"));
        };

        let callargs = CallArgs {
            privileges,
            command: cmd,
        };

        callargs.validate()?;
        Ok(callargs)
    }
}
