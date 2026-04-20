# CLAUDE.md

> **Self-updating**: When you learn something new about this project's patterns, conventions,
> architecture, or coding standards during a task, update this file immediately. Keep it concise
> and authoritative — this is the single source of truth for how to work in this codebase.

## Project

`neomake` is a rusty task-runner CLI — a more powerful alternative to Makefiles. Workflows are
written in **HOCON** only (no YAML), compiled to an **ExecutionPlan** (JSON), then executed.
Executions are DAGs grouped into waves; within a wave all work can run in parallel up to a
bounded worker pool.

**Workspace layout**: `crates/neomake/` is the sole crate. Workspace root at `Cargo.toml`.

```
crates/neomake/src/
  main.rs         Entry point + async tokio runtime. Dispatches clap subcommands.
  args.rs         CLI argument parsing (clap builder). Defines the Command enum + Privilege
                  check (experimental features gating: multiplex, watch).
  reference.rs    Manpages (clap_mangen), markdown docs (clap-markdown), shell completions
                  (clap_complete). Drives `man` and `autocomplete` subcommands.
  workflow.rs    Workflow model + HOCON loader. Structs Workflow / Env / Node / Matrix /
                  MatrixCell / Task / NodeSelector. `Workflow::load()` parses HOCON, validates
                  the version and node-name regex, then deserializes via hocon's serde support.
  plan.rs         ExecutionPlan / Stage / Node / Invocation / Task data types + the Compiler
                  that turns a Workflow into a plan. Compiler does handlebars rendering of task
                  scripts, matrix cartesian-product expansion, and DAG topological grouping
                  into waves (stages). Also implements `list` and `describe` outputs.
  exec.rs         DAG execution engine. Async tokio-based: each wave spawns its batches
                  onto a `JoinSet` bounded by a `Semaphore` (sized by `--workers`). Child
                  processes are `tokio::process::Command` with piped stdout/stderr read
                  line-by-line and emitted as `ExecEvent`s on an mpsc channel. Matrix
                  `parallel=true` spawns one batch per invocation; `parallel=false` glues
                  invocations into a single sequential batch. Supports cooperative cancel
                  via `Cancel` (tripped on user `q` / Ctrl+C). Each `Work` carries an
                  optional `ResolvedContainer`; when present, `run_one` branches through
                  `build_container_command` to spawn `<docker|podman> run --rm -i …` using
                  a shared `RuntimeResolver` (cached on the engine).
  runner.rs       Execution driver selection. `execute()` starts the engine task, then picks
                  between the TUI driver (`tui/`) and the plain streaming driver based on
                  TTY detection and the `--no-tui` flag. Plain driver forwards captured
                  stdout/stderr with a `[node]` prefix and prints per-batch / per-wave
                  summaries; the TUI driver shows the structured live view.
  tui/mod.rs      Terminal setup (alternate screen on stderr, raw mode) and the async event
                  loop driving the execution TUI. `EventStream` for keyboard input; works
                  with piped stdin because crossterm's `use-dev-tty` backend reads keys from
                  /dev/tty rather than stdin.
  tui/app.rs      TUI state: ordered batch list, per-batch status and log buffer (capped at
                  5000 lines), cursor, follow mode, wave counters. `apply(ExecEvent)` drives
                  state transitions from engine events.
  tui/ui.rs       TUI rendering: header with wave progress, two-pane body (batch tree on
                  the left, logs on the right), hotkey bar, help overlay.
  multiplex/mod.rs  Native command multiplexer. `Multiplexer` runs N shell commands with a
                    tokio `JoinSet` bounded by a `Semaphore`; progress and per-task stdout /
                    stderr are emitted as `MuxEvent`s on an mpsc channel. `run()` auto-selects
                    the driver (TUI vs plain). Also hosts the `watch` subcommand (notify-backed
                    fs watcher → regex filter → re-invokes multiplexer). Both commands gated
                    behind `-e/--experimental`.
  multiplex/tui/    Semi-interactive ratatui TUI for `multiplex`, mirroring the execute TUI:
                    task tree left, log pane right, hotkey bar, help overlay. Same keybindings
                    as the execute TUI (j/k, Ctrl+d/u, g/G, f, ?, q, Ctrl+C).

crates/neomake/res/templates/
  min.neomake.conf      Minimal HOCON template (used by `workflow init -t min`)
  max.neomake.conf      Full-featured HOCON template
  python.neomake.conf   Python-centric HOCON template
```

## Design Principles

1. **HOCON-only workflows** — Workflow files are HOCON. YAML is not supported anywhere.
   Plan output is JSON (the data interchange between `plan` and `execute`).
2. **Plan & Execute separation** — `plan` compiles a workflow (resolving env, handlebars
   args, and matrix expansion) into a fully-rendered JSON plan. `execute` reads a plan from
   stdin and runs it. Plans are reviewable, storable, and reproducible.
3. **DAG waves, bounded parallelism** — `pre` edges form a DAG; `determine_order` groups
   nodes into waves (stages). Waves run sequentially; within a wave, a `threadpool::ThreadPool`
   with `--workers` workers bounds concurrency. Errors in a wave abort before the next wave.
4. **Matrix parallelism toggle** — Each node's matrix has `parallel: bool`. `true` means each
   invocation (cartesian cell) is an independent work item submitted to the pool. `false` means
   all tasks for all invocations are serialized into one pool job.
5. **Layered env resolution** — Env vars are merged in this precedence (later overrides earlier):
   `workflow.env → node.env → matrix-cell.env (per invocation) → task.env`. Each level supports
   explicit `vars` and/or a `capture` regex that imports matching host env vars at plan time.
6. **Command multiplexing (experimental)** — `multiplex` runs N shell commands concurrently with
   a `-p/--parallelism` cap, optionally emitting an aggregated JSON result on stdout. The
   multiplexer is **native** to neomake (no external crate): tokio `JoinSet` for parallelism,
   tokio `Semaphore` as budget, `flume` for task events, `crossterm` for the live status view.
7. **Watch (experimental)** — `watch` combines `notify` with the multiplexer: filesystem events
   matching a user regex (`<kind>|<relative-path>`) re-run the configured command set.
8. **Handlebars in scripts** — Task scripts may reference `{{ foo.bar }}`; `plan -a foo.bar=value`
   populates a nested JSON tree used by handlebars in strict mode (missing key → error).
9. **Experimental flag gate** — `multiplex` and `watch` require `-e/--experimental`. Enforced
   centrally in `CallArgs::validate`.
10. **TUI when interactive, plain otherwise.** Both `execute` and `multiplex` auto-select
    their driver from TTY state: TUI when stderr is a TTY, plain streaming driver otherwise.
    Piped stdin is fine — crossterm is built with the `use-dev-tty` + `libc` features, so
    key events come from `/dev/tty` directly instead of stdin (and instead of mio/kqueue,
    which on macOS refuses terminal devices). `--no-tui` forces plain on either subcommand.
    `watch` always uses the plain driver (a re-entering TUI on every fs event would be
    jarring). If event-reader init still fails (e.g. `/dev/tty` unreachable in unusual
    hosts / containers), the runner prints a one-line notice to stderr and falls through to
    plain with no loss of engine output.
10. **Exact node-name discipline** — Node names must match `^[a-zA-Z0-9:_-]+$`. `pre` selectors
    are either exact names (`{ name = "x" }`) or regex (`{ regex = "^build:.*$" }`), deserialized
    as a `#[serde(untagged)]` enum so HOCON `{ name = "a" }` maps cleanly.

## Workflow Feature Reference

### Guards (`when`)
Per-node, evaluated at plan time. Two shapes:
- `when = "{{ args.env }}"` — handlebars template; truthy (non-empty, not `false`/`0`/`no`) → include.
  Renders with non-strict handlebars, so missing args are simply falsy.
- `when { script = "[ -n \"$CI\" ]", shell = "/bin/sh -c" }` — runs a shell command; exit 0 → include.

A node with a falsy guard is omitted from the plan entirely. If a node declared in `inputs`
is guarded out, the compiler errors — depending on a node that won't run is a bug.

### Retries
`retry { attempts, backoff = fixed|linear|exponential, delay_ms }` on either a node or a
task. Task-level overrides node-level. Retries happen at the **work** level (one task × one
invocation), not the whole batch — on failure the same command retries up to `attempts - 1`
more times with the chosen backoff. `Fixed` uses `delay_ms` flat; `Linear` scales by the
attempt number; `Exponential` doubles each attempt (capped at 2^16).

### Input streaming (`inputs`)
List of `NodeSelector`s (same shape as `pre`). Declaring `inputs` implies `pre` — the
producers always run first. When the consumer spawns, its stdin receives a single JSON
blob then EOF:

```json
{
  "producer_matrix": { "0,0": "...", "0,1": "...", "1,0": "...", "1,1": "..." },
  "producer_simple": { "": "..." }
}
```

Each producer is an object keyed by **cell coordinates** (comma-joined matrix indices;
empty string for non-matrix producers). Using a dict rather than an array structurally
guarantees one entry per cell. Each invocation of a matrix *consumer* gets the same full
blob (no automatic cell matching). Aggregation of per-task outputs for a single invocation
uses concatenation in task-declaration order.

### Container encapsulation (`container`)

Optional `container { }` block at four workflow scopes — `workflow`, `node`, `matrix_cell`,
`task` — merged innermost-wins, same precedence chain as `env`. A task runs inside a
container iff the merged result has `image` set and `disabled != true`. The plan baked by
the compiler carries a fully-resolved `Option<ResolvedContainer>` per work item — no CLI
flags control this; every knob lives in the workflow.

```hocon
container {
  image   = "rust:1.94"
  workdir = "/work"                        # container workdir + bind-mount target
  user    = "host"                         # host | root | "UID:GID"
  mount   = true                           # bind host CWD at workdir
  # Runtime = nested enum carrying its own extras. Declare exactly one variant.
  runtime {
    docker { extra_args = [ "--network=host" ] }
  }
}
```

**Runtime is explicit** — exactly two variants (`docker`, `podman`), each carrying its
own `extra_args`. Default when no merge level declares `runtime` is `docker { docker: {
extra_args = [] } }`. There is no runtime auto-detection; asking for `podman` on a
docker-only host fails cleanly at that task's spawn (wave failure → abort).
`RuntimeResolver` caches per-runtime PATH availability so each runtime is probed at most
once per engine run, no matter how many tasks use it.

**Runtime merge semantics**: `extra_args` accumulates across merge levels **only while
consecutive levels declare the same variant**. Switching variants (e.g. workflow = docker,
task = podman) discards the prior variant's extras — you can't "leak" docker-specific
flags onto podman. Within a single variant, order is preserved (workflow → node → cell →
task).

**Runtime-specific extras** are co-located sub-blocks (`docker { extra_args }`,
`podman { extra_args }`). Only the sub-block matching the resolved runtime is applied.
`extra_args` is **additive** across merge levels, preserving order.

**Task-level merge quirk**: a plan `Task::container` is only populated when the task
itself declared a `container` block — otherwise it's left `None` so at execute time the
engine falls through to `Invocation::container` (which already captured
workflow+node+cell merges). This preserves the expected "cell-level override wins for a
plain task, but task-level override beats the cell" semantics.

**Spawn shape** (in `exec.rs::build_container_command`):
```
<runtime> run --rm -i [-v host_cwd:workdir] -w <workdir>
  [--user U:G] [-e K=V ...] [docker|podman.extra_args...]
  <image> <shell_program> <shell_args...> <command>
```
Env vars go via `-e` (not `.envs()`) and workdir via `-w` (not `.current_dir()`).

### HOCON `include`
Use `include "other.conf"` at any object scope. Paths resolve relative to the workflow
file's directory. Substitutions (`${foo}`) work across includes. `Workflow::load` reads
from disk so includes resolve; `Workflow::parse` (test-only) reads from a string and
disables includes.

## HOCON Workflow Format

```hocon
version = "0.0"                  # major.minor must match CLI's (unless "0.0")

env {                            # optional, workflow-global
  capture = "^(HOME|PATH)$"     # regex; matching host env vars are captured at plan time
  vars {                         # explicit vars
    GLOBAL = "value"
  }
}

nodes {
  build {
    description = "Build the project."
    pre         = [              # optional predecessors
      { name  = "lint" },
      { regex = "^gen:.*$" }
    ]
    shell       = "/bin/bash -c" # node-level override
    workdir     = "./crates"     # node-level override
    env { vars { NODE_SCOPED = "1" } }

    matrix {                     # optional n-dim matrix
      parallel = true            # true → each invocation runs in parallel in the pool
      dimensions = [
        [ { env { vars { TARGET = "debug"   } } },
          { env { vars { TARGET = "release" } } } ]
      ]
    }

    tasks = [
      {
        shell   = "python3 -c"   # per-task shell override
        script  = "print('{{ args.msg }}')"   # handlebars placeholders
        env { vars { TASK_SCOPED = "1" } }
        workdir = "./sub"
      }
    ]
  }
}
```

## Subcommands

| Command | Alias | Purpose |
|---------|-------|---------|
| `workflow init -t min\|max\|python -o FILE` | — | Emit a starter HOCON workflow. `-o -` prints to stdout. |
| `workflow schema` | — | Print the JSON schema of the Workflow type. |
| `plan --workflow FILE (-n NAME \| -r REGEX) [-a k=v] [--pretty]` | `p` | Compile workflow → ExecutionPlan JSON on stdout. |
| `execute [-f FILE] [-w N] [--no-stdout] [--no-stderr] [--no-tui] [--summary]` | `exec`, `x` | Run a plan from stdin (default) or `-f FILE`. TUI when stderr is a TTY. `--summary` prints per-wave timings + critical path after exit. |
| `list --workflow FILE [-o json\|json+p\|custom]` | `ls`, `l` | List all nodes. `custom` is human-readable. |
| `lint --workflow FILE [-o text\|json]` | — | Static analysis: unresolved selectors, cycles, bad regex, empty matrices. Exit 1 on any error-level finding. |
| `describe --workflow FILE (-n NAME \| -r REGEX) [--pretty]` | `desc`, `d` | Print DAG waves as JSON. |
| `multiplex -c CMD … [-p N] [--stdout] [--pretty] [--no-tui]` **exp** | `m`, `mp` | Concurrent shell runner; TUI by default when on a terminal. |
| `watch -f REGEX -r DIR -c CMD …` **exp** | — | Re-run commands on filesystem events matching regex. |
| `man -o DIR -f manpages\|markdown` | — | Generate documentation. |
| `autocomplete -o DIR -s bash\|zsh\|fish\|elvish\|powershell` | — | Generate shell completions. |

Experimental commands require `neomake -e …`.

## Coding Standards

### Module structure

Each top-level `crates/neomake/src/*.rs` file has a single responsibility. No loose helper
modules. When a domain grows big enough to need multiple files, promote it to a subdirectory
with `mod.rs` (mirroring `qb`'s `k8s/` and `tui/` pattern). Until then, prefer flat files.

- `workflow.rs` — HOCON parsing + validation + types. No I/O beyond reading the file. No
  execution concerns.
- `plan.rs` — Pure transformation: `Workflow → ExecutionPlan`. Handlebars rendering and DAG
  ordering live here. No process spawning.
- `exec.rs` — Async subprocess orchestration via `tokio::process`. Pure consumer of
  `ExecutionPlan`; streams `ExecEvent`s on an mpsc channel so observation (TUI, plain
  driver) is decoupled from spawning.
- `runner.rs` — Selects the execution driver (TUI vs plain) and wires the engine to it.
- `tui/` — Semi-interactive TUI driver over the engine's event stream.
- `multiplex.rs` — Holds both `multiplex` and `watch` since they share the same multiplexer
  backend.
- `reference.rs` — Output-only (docs + completions).
- `args.rs` — Input-only (clap definitions + parsing into the `Command` enum).

### Enum-driven dispatch

`Command` in `args.rs` is an exhaustive enum — every subcommand has its own variant. `main.rs`
matches on it without a catch-all arm. Adding a subcommand means:
1. Add a variant to `Command`.
2. Extend `ClapArgumentLoader::root_command()` and `load()`.
3. Add a match arm in `main.rs`.
4. If experimental, add a gate in `CallArgs::validate`.

### Patterns to follow

- **Handlebars strict mode**: `Handlebars::new().set_strict_mode(true)` — a missing arg is a
  hard error, not a silent empty string.
- **HOCON enum quirk**: HOCON's serde adapter mishandles externally-tagged enums. Use
  `#[serde(untagged)]` with struct-variant field names matching the HOCON keys (see
  `NodeSelector`). Do **not** use tuple-variants with external tagging for HOCON-parsed types.
- **Env precedence**: Always merge in the order global → node → invocation → task. This is
  hardcoded in `exec.rs`; keep it consistent with anyone reading workflow files.
- **Matrix cell coordinates**: `Invocation::cell` records the per-dimension index of the cell
  that produced it — included in plan output for debuggability; not required at runtime.
- **Cycle detection**: `determine_order` errors with "cycle detected in DAG" when no new leaves
  can be extracted. This also catches unreferenced transitive-only deps because unknown node
  names fail earlier with "node not found".
- **Error aggregation**: `ExecutionEngine::execute` collects all worker errors in a wave and
  returns them joined by `\n`, so the user sees every failing command.

### Style

- Match arms use leading `|` pipes (configured in `.rustfmt.toml`).
- Max line width: 120 chars.
- Prefer `&str` returns for display methods on enums.
- Avoid `unwrap()` — use `unwrap_or`, `unwrap_or_default`, or `?`.
- **No banner / section-divider comments.** Never emit ASCII-dashed dividers like
  `// ---------------------------------------------------------------------------` or any
  variant (`// ===`, `// ###`, boxed headers, nested indented dashes). Use blank lines,
  `impl` boundaries, and well-named items to organize files. If a section genuinely needs a
  label, a normal single-line `//` comment with no flanking dashes is fine — but prefer
  nothing.

## Dependencies

- **hocon** (with `serde-support`) — Workflow file parsing.
- **serde** + **serde_json** — Plan I/O and internal data model.
- **schemars** — `workflow schema` JSON Schema generation.
- **handlebars** (strict mode) — Task script templating.
- **clap** (builder style) + **clap_complete** + **clap_mangen** + **clap-markdown** —
  CLI parsing and documentation generation.
- **tokio** (multi-thread) — async runtime for `multiplex` and `watch`.
- **threadpool** — Synchronous worker pool for `execute` (one pool per wave).
- **fancy-regex** — Workflow node selectors and watch filters.
- **notify** + **signal-hook** — Filesystem watch and signal handling (`watch` subcommand).
- **ratatui** + **crossterm** (event-stream feature) + **futures** — Execution and multiplex
  TUIs.
- **jiff** — Timestamps on multiplexer results (RFC 3339 via serde).
- **itertools** — `multi_cartesian_product` for matrix expansion.

## Build

```sh
cargo build                       # debug
cargo build --release             # release
cargo run -- workflow init -t max # initialize a template
cargo run -- plan -n name | cargo run -- execute -w4
```

## Formatting

**Always run after every edit session:**

```sh
cargo +nightly fmt
```

Formats the whole workspace. Never skip — all code must be formatted before committing.
