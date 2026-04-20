use {
    crate::{
        plan::ExecutionPlan,
        workflow::Workflow,
    },
    anyhow::{
        anyhow,
        Result,
    },
    clap::{
        Arg,
        ArgAction,
    },
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
pub enum Privilege {
    Normal,
    Experimental,
}

#[derive(Debug)]
pub struct CallArgs {
    pub privileges: Privilege,
    pub command: Command,
}

impl CallArgs {
    pub fn validate(&self) -> Result<()> {
        if self.privileges == Privilege::Experimental {
            return Ok(());
        }
        match &self.command {
            | Command::Multiplex { .. } => Err(anyhow!("experimental command: multiplex")),
            | Command::Watch { .. } => Err(anyhow!("experimental command: watch")),
            | _ => Ok(()),
        }
    }
}

#[derive(Debug)]
pub enum ManualFormat {
    Manpages,
    Markdown,
}

#[derive(Debug)]
pub enum WorkflowInitTemplate {
    Min,
    Max,
    Python,
}

impl WorkflowInitTemplate {
    pub fn render(&self) -> String {
        match self {
            | WorkflowInitTemplate::Min => include_str!("../res/templates/min.neomake.conf").to_owned(),
            | WorkflowInitTemplate::Max => include_str!("../res/templates/max.neomake.conf").to_owned(),
            | WorkflowInitTemplate::Python => include_str!("../res/templates/python.neomake.conf").to_owned(),
        }
    }
}

#[derive(Debug)]
pub enum WorkflowInitOutput {
    Stdout,
    File(String),
}

#[derive(Debug)]
pub enum Nodes {
    Arr(HashSet<String>),
    Regex(String),
}

impl Nodes {
    pub fn select(&self, wf: &Workflow) -> Result<HashSet<String>> {
        match self {
            | Self::Arr(v) => Ok(v.clone()),
            | Self::Regex(v) => {
                let re = fancy_regex::Regex::new(v)?;
                let mut out = HashSet::<String>::new();
                for n in wf.nodes.keys() {
                    if re.is_match(n)? {
                        out.insert(n.clone());
                    }
                }
                Ok(out)
            },
        }
    }
}

#[derive(Debug)]
pub enum WorkflowCommand {
    Schema,
    Init {
        template: WorkflowInitTemplate,
        output: WorkflowInitOutput,
    },
}

#[derive(Debug)]
pub enum ListFormat {
    Json { pretty: bool },
    Custom,
}

#[derive(Debug)]
pub enum Command {
    Manual {
        path: String,
        format: ManualFormat,
    },
    Autocomplete {
        path: String,
        shell: clap_complete::Shell,
    },
    Workflow(WorkflowCommand),
    Plan {
        workflow: String,
        nodes: Nodes,
        args: HashMap<String, String>,
        pretty: bool,
    },
    Execute {
        plan: ExecutionPlan,
        workers: usize,
        no_stdout: bool,
        no_stderr: bool,
        tui: crate::runner::TuiMode,
        summary: bool,
    },
    List {
        workflow: String,
        format: ListFormat,
    },
    Lint {
        workflow: String,
        json: bool,
    },
    Describe {
        workflow: String,
        nodes: Nodes,
        pretty: bool,
    },
    Multiplex {
        program: Vec<String>,
        stdout_pretty: bool,
        print_stdout: bool,
        commands: Vec<String>,
        parallelism: Option<usize>,
        tui: crate::runner::TuiMode,
    },
    Watch {
        filter: String,
        root: String,
        program: Vec<String>,
        commands: Vec<String>,
        parallelism: Option<usize>,
    },
}

pub struct ClapArgumentLoader {}

impl ClapArgumentLoader {
    pub fn root_command() -> clap::Command {
        clap::Command::new("neomake")
            .version(env!("CARGO_PKG_VERSION"))
            .about("A rusty makefile alternative / task runner (HOCON workflows).")
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
                    .arg(Arg::new("out").short('o').long("out").required(true))
                    .arg(
                        Arg::new("format")
                            .short('f')
                            .long("format")
                            .value_parser(["manpages", "markdown"])
                            .required(true),
                    ),
            )
            .subcommand(
                clap::Command::new("autocomplete")
                    .about("Renders shell completion scripts.")
                    .arg(Arg::new("out").short('o').long("out").required(true))
                    .arg(
                        Arg::new("shell")
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
                                    .default_value("min")
                                    .value_parser(["min", "max", "python"]),
                            )
                            .arg(
                                Arg::new("output")
                                    .short('o')
                                    .long("output")
                                    .help("The file to write the template to. \"-\" renders to stdout.")
                                    .default_value("./neomake.conf"),
                            ),
                    )
                    .subcommand(clap::Command::new("schema").about("Renders the workflow JSON schema to stdout.")),
            )
            .subcommand(
                clap::Command::new("plan")
                    .about("Creates an execution plan (JSON) from a workflow.")
                    .visible_aliases(["p"])
                    .arg(
                        Arg::new("workflow")
                            .long("workflow")
                            .help("The workflow file to use.")
                            .default_value("./neomake.conf"),
                    )
                    .arg(
                        Arg::new("node")
                            .short('n')
                            .long("node")
                            .action(ArgAction::Append)
                            .conflicts_with("regex")
                            .required_unless_present("regex")
                            .help("Add a node to the plan."),
                    )
                    .arg(
                        Arg::new("regex")
                            .short('r')
                            .long("regex")
                            .conflicts_with("node")
                            .required_unless_present("node")
                            .help("Add nodes by regex."),
                    )
                    .arg(
                        Arg::new("arg")
                            .short('a')
                            .long("arg")
                            .action(ArgAction::Append)
                            .help("Specifies a handlebars key=value pair (dot-notation for nesting)."),
                    )
                    .arg(
                        Arg::new("pretty")
                            .long("pretty")
                            .num_args(0)
                            .help("Pretty-print the JSON plan."),
                    ),
            )
            .subcommand(
                clap::Command::new("execute")
                    .about("Executes a plan (JSON) read from stdin or a file.")
                    .visible_aliases(["exec", "x"])
                    .arg(Arg::new("file").short('f').long("file").help(
                        "Read the plan from this file instead of stdin. Required for TUI mode when stdin is piped.",
                    ))
                    .arg(
                        Arg::new("workers")
                            .short('w')
                            .long("workers")
                            .help("Worker thread count.")
                            .default_value("1"),
                    )
                    .arg(
                        Arg::new("no-stdout")
                            .long("no-stdout")
                            .num_args(0)
                            .help("Suppress child stdout."),
                    )
                    .arg(
                        Arg::new("no-stderr")
                            .long("no-stderr")
                            .num_args(0)
                            .help("Suppress child stderr."),
                    )
                    .arg(
                        Arg::new("no-tui")
                            .long("no-tui")
                            .num_args(0)
                            .help("Disable the TUI. Default: TUI when stderr is a TTY, plain otherwise."),
                    )
                    .arg(Arg::new("summary").long("summary").num_args(0).help(
                        "After the run, print a timing summary (wave durations, slowest batches, critical path).",
                    )),
            )
            .subcommand(
                clap::Command::new("describe")
                    .about("Prints DAG waves as JSON.")
                    .visible_aliases(["desc", "d"])
                    .arg(Arg::new("workflow").long("workflow").default_value("./neomake.conf"))
                    .arg(
                        Arg::new("node")
                            .short('n')
                            .long("node")
                            .action(ArgAction::Append)
                            .conflicts_with("regex")
                            .required_unless_present("regex"),
                    )
                    .arg(
                        Arg::new("regex")
                            .short('r')
                            .long("regex")
                            .conflicts_with("node")
                            .required_unless_present("node"),
                    )
                    .arg(Arg::new("pretty").long("pretty").num_args(0)),
            )
            .subcommand(
                clap::Command::new("list")
                    .about("Lists all available nodes.")
                    .visible_aliases(["ls", "l"])
                    .arg(Arg::new("workflow").long("workflow").default_value("./neomake.conf"))
                    .arg(
                        Arg::new("output")
                            .short('o')
                            .long("output")
                            .value_parser(["json", "json+p", "custom"])
                            .default_value("json"),
                    ),
            )
            .subcommand(
                clap::Command::new("lint")
                    .about("Validate a workflow — reports unresolved selectors, cycles, bad regex, and empty matrices.")
                    .arg(Arg::new("workflow").long("workflow").default_value("./neomake.conf"))
                    .arg(
                        Arg::new("output")
                            .short('o')
                            .long("output")
                            .help("Output format. 'text' is human-readable; 'json' is machine-parseable.")
                            .value_parser(["text", "json"])
                            .default_value("text"),
                    ),
            )
            .subcommand(
                clap::Command::new("multiplex")
                    .about("Multiplex shell commands with bounded parallelism.")
                    .visible_aliases(["m", "mp"])
                    .args([
                        Arg::new("program")
                            .long("program")
                            .help("Program invoked for each command.")
                            .default_value("/bin/sh -c"),
                        Arg::new("stdout")
                            .long("stdout")
                            .num_args(0)
                            .help("Emit aggregate stdout of all processes as JSON."),
                        Arg::new("pretty").long("pretty").num_args(0),
                        Arg::new("parallelism")
                            .long("parallelism")
                            .short('p')
                            .help("Maximum number of concurrent processes."),
                        Arg::new("command")
                            .short('c')
                            .long("command")
                            .action(ArgAction::Append)
                            .help("A command to run."),
                        Arg::new("no-tui")
                            .long("no-tui")
                            .num_args(0)
                            .help("Disable the TUI. Default: TUI when stdin and stderr are TTYs."),
                    ]),
            )
            .subcommand(
                clap::Command::new("watch")
                    .about("Run commands on filesystem events matching a regex.")
                    .args([
                        Arg::new("filter").short('f').long("filter").required(true),
                        Arg::new("root").short('r').long("root").default_value("./"),
                        Arg::new("program").long("program").default_value("/bin/sh -c"),
                        Arg::new("parallelism")
                            .short('p')
                            .long("parallelism")
                            .default_value("1"),
                        Arg::new("command").short('c').long("command").action(ArgAction::Append),
                    ]),
            )
    }

    pub fn load() -> Result<CallArgs> {
        let m = Self::root_command().get_matches();
        let privileges = if m.get_flag("experimental") {
            Privilege::Experimental
        } else {
            Privilege::Normal
        };

        fn parse_nodes(x: &clap::ArgMatches) -> Nodes {
            match x.get_many::<String>("node") {
                | Some(v) => Nodes::Arr(HashSet::<String>::from_iter(v.map(|s| s.to_owned()))),
                | None => Nodes::Regex(x.get_one::<String>("regex").unwrap().to_owned()),
            }
        }

        fn parse_args_map(x: &clap::ArgMatches) -> HashMap<String, String> {
            let mut out = HashMap::new();
            if let Some(args) = x.get_many::<String>("arg") {
                for a in args {
                    let spl: Vec<&str> = a.splitn(2, '=').collect();
                    if spl.len() == 2 {
                        out.insert(spl[0].to_owned(), spl[1].to_owned());
                    }
                }
            }
            out
        }

        let cmd = if let Some(x) = m.subcommand_matches("man") {
            Command::Manual {
                path: x.get_one::<String>("out").unwrap().clone(),
                format: match x.get_one::<String>("format").unwrap().as_str() {
                    | "manpages" => ManualFormat::Manpages,
                    | "markdown" => ManualFormat::Markdown,
                    | _ => return Err(anyhow!("invalid format")),
                },
            }
        } else if let Some(x) = m.subcommand_matches("autocomplete") {
            Command::Autocomplete {
                path: x.get_one::<String>("out").unwrap().clone(),
                shell: clap_complete::Shell::from_str(x.get_one::<String>("shell").unwrap()).unwrap(),
            }
        } else if let Some(x) = m.subcommand_matches("workflow") {
            if let Some(y) = x.subcommand_matches("init") {
                Command::Workflow(WorkflowCommand::Init {
                    template: match y.get_one::<String>("template").unwrap().as_str() {
                        | "min" => WorkflowInitTemplate::Min,
                        | "max" => WorkflowInitTemplate::Max,
                        | "python" => WorkflowInitTemplate::Python,
                        | _ => return Err(anyhow!("invalid template")),
                    },
                    output: match y.get_one::<String>("output").unwrap().as_str() {
                        | "-" => WorkflowInitOutput::Stdout,
                        | s => WorkflowInitOutput::File(s.to_owned()),
                    },
                })
            } else if x.subcommand_matches("schema").is_some() {
                Command::Workflow(WorkflowCommand::Schema)
            } else {
                return Err(anyhow!("unknown workflow subcommand"));
            }
        } else if let Some(x) = m.subcommand_matches("plan") {
            Command::Plan {
                workflow: x.get_one::<String>("workflow").unwrap().clone(),
                nodes: parse_nodes(x),
                args: parse_args_map(x),
                pretty: x.get_flag("pretty"),
            }
        } else if let Some(x) = m.subcommand_matches("execute") {
            let plan = match x.get_one::<String>("file") {
                | Some(path) => std::fs::read_to_string(path)?,
                | None => {
                    let mut s = String::new();
                    std::io::stdin().read_to_string(&mut s)?;
                    s
                },
            };
            Command::Execute {
                plan: serde_json::from_str::<ExecutionPlan>(&plan)?,
                workers: x.get_one::<String>("workers").unwrap().parse::<usize>()?,
                no_stdout: x.get_flag("no-stdout"),
                no_stderr: x.get_flag("no-stderr"),
                tui: if x.get_flag("no-tui") {
                    crate::runner::TuiMode::Off
                } else {
                    crate::runner::TuiMode::Auto
                },
                summary: x.get_flag("summary"),
            }
        } else if let Some(x) = m.subcommand_matches("describe") {
            Command::Describe {
                workflow: x.get_one::<String>("workflow").unwrap().clone(),
                nodes: parse_nodes(x),
                pretty: x.get_flag("pretty"),
            }
        } else if let Some(x) = m.subcommand_matches("list") {
            Command::List {
                workflow: x.get_one::<String>("workflow").unwrap().clone(),
                format: match x.get_one::<String>("output").unwrap().as_str() {
                    | "json" => ListFormat::Json { pretty: false },
                    | "json+p" => ListFormat::Json { pretty: true },
                    | "custom" => ListFormat::Custom,
                    | _ => return Err(anyhow!("unknown list format")),
                },
            }
        } else if let Some(x) = m.subcommand_matches("lint") {
            Command::Lint {
                workflow: x.get_one::<String>("workflow").unwrap().clone(),
                json: x.get_one::<String>("output").map(|s| s == "json").unwrap_or(false),
            }
        } else if let Some(x) = m.subcommand_matches("multiplex") {
            let commands = x
                .get_many::<String>("command")
                .unwrap_or_default()
                .cloned()
                .collect::<Vec<_>>();
            let program = x
                .get_one::<String>("program")
                .unwrap()
                .split_whitespace()
                .map(|s| s.to_owned())
                .collect::<Vec<_>>();
            Command::Multiplex {
                program,
                stdout_pretty: x.get_flag("pretty"),
                print_stdout: x.get_flag("stdout"),
                commands,
                parallelism: x.get_one::<String>("parallelism").map(|v| v.parse::<usize>().unwrap()),
                tui: if x.get_flag("no-tui") {
                    crate::runner::TuiMode::Off
                } else {
                    crate::runner::TuiMode::Auto
                },
            }
        } else if let Some(x) = m.subcommand_matches("watch") {
            let commands = x
                .get_many::<String>("command")
                .unwrap_or_default()
                .cloned()
                .collect::<Vec<_>>();
            let program = x
                .get_one::<String>("program")
                .unwrap()
                .split_whitespace()
                .map(|s| s.to_owned())
                .collect::<Vec<_>>();
            Command::Watch {
                filter: x.get_one::<String>("filter").unwrap().clone(),
                root: x.get_one::<String>("root").unwrap().clone(),
                program,
                commands,
                parallelism: x.get_one::<String>("parallelism").map(|v| v.parse::<usize>().unwrap()),
            }
        } else {
            return Err(anyhow!("unknown command"));
        };

        let call = CallArgs {
            privileges,
            command: cmd,
        };
        call.validate()?;
        Ok(call)
    }
}
