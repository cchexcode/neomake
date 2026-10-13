use {
    crate::{
        plan::ExecutionPlan,
        workflow::Workflow,
    },
    anyhow::{
        anyhow,
        Context,
        Result,
    },
    clap::{
        Arg,
        ArgAction,
        ArgMatches,
    },
    std::{
        collections::{
            HashMap,
            HashSet,
        },
        io::Read,
        num::NonZeroUsize,
        path::PathBuf,
        str::FromStr,
    },
};

#[derive(Debug, Eq, PartialEq)]
pub enum Privilege {
    Normal,
    Experimental,
}

/// Whether the caller asked for a TUI. The tty check stays in `main`.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TuiMode {
    Auto,
    Off,
}

impl TuiMode {
    pub fn requested(self) -> bool {
        matches!(self, Self::Auto)
    }
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
    File(PathBuf),
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
        path: PathBuf,
        format: ManualFormat,
    },
    Autocomplete {
        path: PathBuf,
        shell: clap_complete::Shell,
    },
    Workflow(WorkflowCommand),
    Plan {
        workflow: PathBuf,
        nodes: Nodes,
        args: HashMap<String, String>,
        pretty: bool,
    },
    Execute {
        plan: ExecutionPlan,
        workers: NonZeroUsize,
        no_stdout: bool,
        no_stderr: bool,
        tui: TuiMode,
        summary: bool,
    },
    List {
        workflow: PathBuf,
        format: ListFormat,
    },
    Lint {
        workflow: PathBuf,
        json: bool,
    },
    Describe {
        workflow: PathBuf,
        nodes: Nodes,
        pretty: bool,
    },
    Multiplex {
        program: Vec<String>,
        stdout_pretty: bool,
        print_stdout: bool,
        commands: Vec<String>,
        parallelism: Option<NonZeroUsize>,
        tui: TuiMode,
    },
    Watch {
        filter: String,
        root: PathBuf,
        program: Vec<String>,
        commands: Vec<String>,
        parallelism: NonZeroUsize,
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
                            .help("Maximum number of concurrent batches.")
                            .value_parser(clap::value_parser!(NonZeroUsize))
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
                            .help("Maximum number of concurrent processes.")
                            .value_parser(clap::value_parser!(NonZeroUsize)),
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
                            .value_parser(clap::value_parser!(NonZeroUsize))
                            .default_value("1"),
                        Arg::new("command").short('c').long("command").action(ArgAction::Append),
                    ]),
            )
    }

    pub fn load() -> Result<CallArgs> {
        let matches = Self::root_command().get_matches();
        let privileges = if matches.get_flag("experimental") {
            Privilege::Experimental
        } else {
            Privilege::Normal
        };
        let command = parse_command(&matches)?;
        let call = CallArgs { privileges, command };
        call.validate()?;
        Ok(call)
    }
}

fn parse_command(matches: &ArgMatches) -> Result<Command> {
    if let Some(sub) = matches.subcommand_matches("man") {
        return Ok(Command::Manual {
            path: required_path(sub, "out")?,
            format: match required_str(sub, "format")? {
                | "manpages" => ManualFormat::Manpages,
                | "markdown" => ManualFormat::Markdown,
                | other => return Err(anyhow!("invalid format `{other}`")),
            },
        });
    }
    if let Some(sub) = matches.subcommand_matches("autocomplete") {
        let shell = required_str(sub, "shell")?;
        return Ok(Command::Autocomplete {
            path: required_path(sub, "out")?,
            shell: clap_complete::Shell::from_str(shell).map_err(|_| anyhow!("invalid shell `{shell}`"))?,
        });
    }
    if let Some(sub) = matches.subcommand_matches("workflow") {
        if let Some(init) = sub.subcommand_matches("init") {
            let template = match required_str(init, "template")? {
                | "min" => WorkflowInitTemplate::Min,
                | "max" => WorkflowInitTemplate::Max,
                | "python" => WorkflowInitTemplate::Python,
                | other => return Err(anyhow!("invalid template `{other}`")),
            };
            let output = match required_str(init, "output")? {
                | "-" => WorkflowInitOutput::Stdout,
                | path => WorkflowInitOutput::File(PathBuf::from(path)),
            };
            return Ok(Command::Workflow(WorkflowCommand::Init { template, output }));
        }
        if sub.subcommand_matches("schema").is_some() {
            return Ok(Command::Workflow(WorkflowCommand::Schema));
        }
        return Err(anyhow!("unknown workflow subcommand"));
    }
    if let Some(sub) = matches.subcommand_matches("plan") {
        return Ok(Command::Plan {
            workflow: required_path(sub, "workflow")?,
            nodes: parse_nodes(sub)?,
            args: parse_args_map(sub)?,
            pretty: sub.get_flag("pretty"),
        });
    }
    if let Some(sub) = matches.subcommand_matches("execute") {
        let raw = match sub.get_one::<String>("file") {
            | Some(path) => std::fs::read_to_string(path).with_context(|| format!("failed to read plan {path}"))?,
            | None => {
                let mut buf = String::new();
                std::io::stdin()
                    .read_to_string(&mut buf)
                    .context("failed to read plan from stdin")?;
                buf
            },
        };
        return Ok(Command::Execute {
            plan: serde_json::from_str::<ExecutionPlan>(&raw)
                .context("failed to parse execution plan; re-run `neomake plan`")?,
            workers: required_nonzero(sub, "workers")?,
            no_stdout: sub.get_flag("no-stdout"),
            no_stderr: sub.get_flag("no-stderr"),
            tui: tui_mode(sub),
            summary: sub.get_flag("summary"),
        });
    }
    if let Some(sub) = matches.subcommand_matches("describe") {
        return Ok(Command::Describe {
            workflow: required_path(sub, "workflow")?,
            nodes: parse_nodes(sub)?,
            pretty: sub.get_flag("pretty"),
        });
    }
    if let Some(sub) = matches.subcommand_matches("list") {
        let format = match required_str(sub, "output")? {
            | "json" => ListFormat::Json { pretty: false },
            | "json+p" => ListFormat::Json { pretty: true },
            | "custom" => ListFormat::Custom,
            | other => return Err(anyhow!("unknown list format `{other}`")),
        };
        return Ok(Command::List {
            workflow: required_path(sub, "workflow")?,
            format,
        });
    }
    if let Some(sub) = matches.subcommand_matches("lint") {
        return Ok(Command::Lint {
            workflow: required_path(sub, "workflow")?,
            json: sub.get_one::<String>("output").map(|s| s == "json").unwrap_or(false),
        });
    }
    if let Some(sub) = matches.subcommand_matches("multiplex") {
        return Ok(Command::Multiplex {
            program: parse_program(sub)?,
            stdout_pretty: sub.get_flag("pretty"),
            print_stdout: sub.get_flag("stdout"),
            commands: collect_commands(sub),
            parallelism: sub.get_one::<NonZeroUsize>("parallelism").copied(),
            tui: tui_mode(sub),
        });
    }
    if let Some(sub) = matches.subcommand_matches("watch") {
        return Ok(Command::Watch {
            filter: required_str(sub, "filter")?.to_owned(),
            root: required_path(sub, "root")?,
            program: parse_program(sub)?,
            commands: collect_commands(sub),
            parallelism: required_nonzero(sub, "parallelism")?,
        });
    }
    Err(anyhow!("unknown command"))
}

fn required_str<'a>(matches: &'a ArgMatches, name: &str) -> Result<&'a str> {
    matches
        .get_one::<String>(name)
        .map(String::as_str)
        .ok_or_else(|| anyhow!("missing argument `{name}`"))
}

fn required_path(matches: &ArgMatches, name: &str) -> Result<PathBuf> {
    Ok(PathBuf::from(required_str(matches, name)?))
}

fn required_nonzero(matches: &ArgMatches, name: &str) -> Result<NonZeroUsize> {
    matches
        .get_one::<NonZeroUsize>(name)
        .copied()
        .ok_or_else(|| anyhow!("missing argument `{name}`"))
}

fn tui_mode(matches: &ArgMatches) -> TuiMode {
    if matches.get_flag("no-tui") {
        TuiMode::Off
    } else {
        TuiMode::Auto
    }
}

fn parse_nodes(matches: &ArgMatches) -> Result<Nodes> {
    match matches.get_many::<String>("node") {
        | Some(names) => Ok(Nodes::Arr(names.cloned().collect())),
        | None => {
            let regex = required_str(matches, "regex")?;
            fancy_regex::Regex::new(regex).map_err(|err| anyhow!("invalid node regex `{regex}`: {err}"))?;
            Ok(Nodes::Regex(regex.to_owned()))
        },
    }
}

fn parse_args_map(matches: &ArgMatches) -> Result<HashMap<String, String>> {
    let mut out = HashMap::new();
    let Some(args) = matches.get_many::<String>("arg") else {
        return Ok(out);
    };
    for arg in args {
        let Some((key, value)) = arg.split_once('=') else {
            return Err(anyhow!("arg `{arg}` must be key=value"));
        };
        if key.is_empty() || key.split('.').any(|part| part.is_empty()) {
            return Err(anyhow!("arg key `{key}` is empty or has an empty segment"));
        }
        if out.insert(key.to_owned(), value.to_owned()).is_some() {
            return Err(anyhow!("duplicate arg key `{key}`"));
        }
    }
    Ok(out)
}

fn collect_commands(matches: &ArgMatches) -> Vec<String> {
    matches
        .get_many::<String>("command")
        .unwrap_or_default()
        .cloned()
        .collect()
}

fn parse_program(matches: &ArgMatches) -> Result<Vec<String>> {
    let raw = required_str(matches, "program")?;
    let program: Vec<String> = raw.split_whitespace().map(str::to_owned).collect();
    if program.is_empty() {
        return Err(anyhow!("--program must include an executable"));
    }
    Ok(program)
}
