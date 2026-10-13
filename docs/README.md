# neomake

![](neomake.png)

[![dependency status](https://deps.rs/repo/github/cchexcode/neomake/status.svg)](https://deps.rs/repo/github/cchexcode/neomake)

`neomake` is a rusty task-runner CLI — a more powerful alternative to Makefiles. Workflows
are written in **HOCON**, compiled to a reviewable **execution plan (JSON)**, then executed
as a **DAG of parallel waves**. A native command multiplexer and filesystem watcher round out
the toolbox.

## Project state

Released and actively maintained.

## Features

- **DAG execution, in waves.** Nodes declare prerequisites; neomake topologically groups them
  into waves. Waves run sequentially; all work inside a wave runs concurrently up to
  `--workers N`.
- **Plan & Execute as two phases.** `neomake plan` compiles a workflow to a fully-rendered
  JSON plan — handlebars templates resolved, env captured, matrix expanded. `neomake execute`
  reads that plan from stdin (or `--file`) and runs it. Plans are reviewable, storable, and
  reproducible.
- **Interactive TUI by default.** Running `neomake execute` from a terminal opens a
  ratatui-based observation UI: a per-batch tree with status glyphs and timings on the left,
  a scrollable log pane (follow mode + regex-free filtering by cursor) on the right, `Ctrl+C`
  or `q` to cancel. Piped usage (`plan | execute`) auto-falls back to a plain streaming
  driver, so scripts and CI keep working unchanged. `--no-tui` forces the plain driver.
- **Matrix invocations.** Each node may declare an n-dimensional matrix; neomake runs the node
  once per cell of the cartesian product. `parallel: true` dispatches cells independently into
  the worker pool; `parallel: false` runs them in sequence.
- **Layered environment.** Env vars compose across four scopes in this precedence
  (inner overrides outer): **workflow → node → matrix cell → task**. Each scope supports
  explicit `vars` and/or a `capture` regex that imports matching host env vars at plan time.
- **Handlebars templating.** Task scripts may reference `{{ args.foo }}`; pass values on the
  command line with `-a foo=value` (dot-notation produces nested objects). Strict mode: a
  missing key is a hard error.
- **HOCON-only workflow format.** No YAML. HOCON's comments, substitutions, `include`
  directives, and object-merge semantics are a clean fit for configuration. Split large
  workflows with `include "shared.conf"`; paths resolve relative to the workflow file.
- **Conditional nodes (`when`).** Guard a node with a handlebars template or a shell
  script — falsy guards drop the node from the plan entirely at compile time.
- **Retries.** `retry { attempts, backoff, delay_ms }` on a node or a single task,
  re-runs the same command with fixed / linear / exponential backoff.
- **Inputs streaming.** Declare `inputs = [ { name = "producer" } ]` and the consumer
  gets the producer's captured stdout piped to its stdin as a single JSON blob —
  matrix cells and all. Lets you chain nodes without temp files.
- **Static analysis (`lint`).** Validates a workflow: unresolved selectors, bad regex,
  empty matrices, cycles — exits non-zero on any real error.
- **Container encapsulation.** Declare `container { image = "alpine" }` at workflow,
  node, matrix-cell, or task scope (innermost-wins merge) to route those tasks through
  `docker run` / `podman run` with the host CWD bind-mounted. Pick the runtime
  explicitly with `runtime { docker { … } }` or `runtime { podman { … } }` (default docker, no autoselect).
  No CLI flags — everything lives in the workflow file.
- **Native command multiplexing.** `neomake multiplex` runs N shell commands concurrently
  with a bounded parallelism budget. When run from a terminal it shows a ratatui-based TUI
  — task tree with live status, log pane per task, follow mode, help overlay — mirroring
  the execute TUI. Non-TTY invocation (scripts, CI, pipes) falls back to a line-oriented
  plain driver. Either driver can emit a structured JSON result on stdout with `--stdout`.
  Implemented in-tree — no external crate.
- **Filesystem watch.** `neomake watch` runs a command set whenever a filesystem event
  matches a regex (`<event_kind>|<relative_path>`). Useful for rebuild-on-save loops.
- **First-class docs.** Ship manpages, markdown docs, and shell completions (`bash`, `zsh`,
  `fish`, `elvish`, `powershell`) via `neomake man` and `neomake autocomplete`.

## Installation

`neomake` is distributed through `cargo`.

1. Latest stable release:
   ```bash
   cargo install neomake
   ```
2. Bleeding edge from `master`:
   ```bash
   cargo install --git https://github.com/cchexcode/neomake.git
   ```

## Quick start

Generate a starter workflow:

```bash
neomake workflow init -t python -o ./neomake.conf
```

Run a node with the default single-worker pool (piped — plain streaming output):

```bash
neomake plan -n count | neomake x
```

Scale up to 4 concurrent workers:

```bash
neomake plan -n count | neomake x -w4
```

The TUI opens automatically whenever stderr is a terminal — including the piped workflow
above. To work from a pre-built plan (no pipe):

```bash
neomake plan -n count -o plan.json
neomake execute --file plan.json -w4
```

Force the plain driver even when stderr is a TTY:

```bash
neomake execute --file plan.json --no-tui
```

## Workflow format (HOCON)

A complete example:

```hocon
version = "0.0"

env {
  capture = "^(HOME|PATH)$"       # regex; matching host env vars captured at plan time
  vars { GLOBAL = "from-workflow" }
}

nodes {
  lint {
    tasks = [ { script = "cargo clippy --all-targets -- -D warnings" } ]
  }

  build {
    description = "Build in debug + release, in parallel."
    pre = [ { name = "lint" } ]

    matrix {
      parallel = true
      dimensions = [
        [
          { env { vars { PROFILE = ""          } } },
          { env { vars { PROFILE = "--release" } } }
        ]
      ]
    }
    tasks = [
      { script = "cargo build $PROFILE" }
    ]
  }

  release {
    pre = [ { regex = "^build$" } ]
    tasks = [
      { script = "gh release create {{ args.version }} --target=master" }
    ]
  }
}
```

Node names must match `^[a-zA-Z0-9:_-]+$`. `pre` entries are either
`{ name = "exact" }` or `{ regex = "^pattern$" }`.

## Commands

| Command                                 | Alias            | Purpose                                                             |
|-----------------------------------------|------------------|---------------------------------------------------------------------|
| `workflow init -t TEMPLATE -o FILE`     |                  | Write a starter workflow (`min`, `max`, `python`). `-o -` = stdout. |
| `workflow schema`                       |                  | Print the workflow JSON Schema.                                     |
| `plan --workflow F (-n NAME \| -r RE)`  | `p`              | Compile workflow → execution-plan JSON on stdout.                   |
| `execute [-f PATH] [-w N] [--no-stdout] [--no-stderr] [--no-tui]` | `exec`, `x` | Run a plan. Reads stdin or `--file`. TUI when stderr is a TTY. |
| `list --workflow F [-o json\|json+p\|custom]` | `ls`, `l`  | List all nodes.                                                     |
| `lint --workflow F [-o text\|json]`     |                  | Validate the workflow (exits 1 on errors).                          |
| `describe --workflow F (-n NAME \| -r RE) [--pretty]` | `desc`, `d` | Print DAG waves as JSON.                                |
| `multiplex -c CMD… [-p N] [--stdout] [--no-tui]` | `m`, `mp` | Concurrent shell-command runner (experimental). TUI by default.     |
| `watch -f REGEX -r DIR -c CMD…`         |                  | Re-run commands on matching filesystem events (experimental).       |
| `man -o DIR -f manpages\|markdown`      |                  | Generate documentation.                                             |
| `autocomplete -o DIR -s SHELL`          |                  | Generate shell completions.                                         |

Experimental commands require `-e/--experimental`:

```bash
neomake -e multiplex -c 'cargo test' -c 'cargo clippy' -p2 --stdout --pretty
```

## Conditional nodes, retries, and input streaming

```hocon
version = "0.0"

include "shared.conf"          # splits are OK; paths are relative to this file

nodes {
  fetch {
    tasks = [ { script = "curl -s https://example.com/data.json" } ]
  }

  transform {
    inputs = [ { name = "fetch" } ]            # implies pre; pipes fetch's stdout on stdin
    tasks = [
      { script = "jq -r '.items[] | @json'" }
    ]
  }

  publish {
    when  = "{{ args.env }}"                   # skipped unless -a args.env=prod
    pre   = [ { name = "transform" } ]
    retry { attempts = 3, backoff = "exponential", delay_ms = 500 }
    tasks = [ { script = "upload-to-s3 ..." } ]
  }
}
```

The consumer (`transform`) receives on stdin a JSON object keyed by producer → cell:

```json
{
  "fetch": {
    "": "…the raw curl output…"
  }
}
```

For a matrix producer, each cell is its own key (comma-joined coordinates). Single-
invocation producers use the empty string — structurally guaranteed unique, so there's
one entry per cell no matter the shape:

```json
{
  "build_matrix": {
    "0,0": "…build for debug/amd64…",
    "0,1": "…build for debug/arm64…",
    "1,0": "…build for release/amd64…",
    "1,1": "…build for release/arm64…"
  }
}
```

```sh
jq -r '.fetch[""]'                          # simple producer
jq -r '.build_matrix["1,0"]'                # specific matrix cell
jq -r '.build_matrix | to_entries[] | "\(.key): \(.value[0:40])"'   # iterate
```

### Lint and summary

Catch misconfigurations before running anything:

```bash
$ neomake lint
error [a] pre[0] references missing node "missing"
warn  [b] inputs[0] regex "^no-match$" matches no nodes
```

After execution, get a post-run breakdown:

```bash
$ neomake plan -n publish | neomake execute --no-tui --summary
…
── summary ────────────────────────────────
total: 412 ms · 3/3 batches ok
  wave 0: 120 ms (1 batches)
  wave 1:  64 ms (1 batches)
  wave 2: 226 ms (1 batches)
slowest batches:
  ✓ publish — 226 ms (wave 2)
  ✓ fetch   — 120 ms (wave 0)
  ✓ transform —  64 ms (wave 1)
critical path (410 ms):
  → fetch (120 ms)
  → transform (64 ms)
  → publish (226 ms)
───────────────────────────────────────────
```

## DAG execution

Consider a workflow with five nodes:

```hocon
nodes {
  A { tasks = [] }
  B { tasks = [] }
  C { pre = [ { name = "A" } ],                   tasks = [] }
  D { pre = [ { name = "B" } ],                   tasks = [] }
  E { pre = [ { name = "A" }, { name = "D" } ],   tasks = [] }
}
```

Describing the execution of `C` and `E`:

```bash
neomake describe -n C -n E --pretty
```

```json
{
  "stages": [
    ["A", "B"],
    ["C", "D"],
    ["E"]
  ]
}
```

Wave 1 runs `A` and `B` concurrently. Wave 2 runs `C` and `D` concurrently. Wave 3 runs `E`.
Within each wave, `--workers N` bounds the pool size. Cycles are detected and rejected.
Every node appears at most once per run regardless of how many times it is reached as a
transitive prerequisite.

## Environment variable precedence

Later scopes override earlier ones, key by key. The compiler bakes the merged map
into each planned task; execute does not merge again:

```
workflow.env   →   node.env   →   matrix-cell.env (per invocation)   →   task.env
```

Within each scope, explicit `vars` are merged first, and any env vars matching `capture`
(a regex over the host env keyset) are overlaid on top.

## Execution TUI

`neomake execute` is auto-interactive: when stderr is a TTY, the execution
runs under a ratatui-based TUI rendered to the alternate stderr screen. Layout:

```
┌ neomake · wave 2/3 · 5/8 batches · 12s ──────────────────────────────────────┐
│ Batches                           │ logs: build[release] [follow]             │
│  w0  ✓ lint              87ms     │ $ cargo build --release                   │
│  w1  ✓ build[debug]     912ms     │    Compiling neomake v0.0.0 ...           │
│  w1  ◐ build[release]             │    Finished `release` profile [...] ...   │
│  w2  ● package                    │                                           │
│ ...                               │                                           │
├──────────────────────────────────────────────────────────────────────────────┤
│ j/k nav · Ctrl+d/u page · g/G top/bot · f follow · ? help · q quit           │
└──────────────────────────────────────────────────────────────────────────────┘
```

Keybindings: `j/k` / `Up/Down` move the cursor; `Ctrl+d/u` and `PgUp/PgDn` page the logs;
`g/G` / `Home/End` jump to top/bottom of the log pane; `f` toggles follow mode; `?` shows
help; `q` quits (cancels a running plan if still in progress); `Ctrl+C` cancels immediately.

The engine keeps running in the background on a tokio runtime — each batch is a
`tokio::process::Command` with piped stdout/stderr captured line-by-line and fed into the
TUI. Matrix `parallel = true` dispatches each invocation as its own scheduling unit, so the
TUI shows concurrent progress across cells.

### How the TUI reads keys with piped stdin

`neomake plan | neomake execute` enters the TUI just like `execute --file`. Crossterm is
built with the `use-dev-tty` + `libc` features, so the event reader opens `/dev/tty`
directly instead of going through stdin or mio/kqueue (the latter refuses terminal devices
on macOS). If `/dev/tty` is unreachable — some headless containers, for example —
`neomake execute` prints a one-line notice to stderr and falls back to the plain driver
without losing any output. `--no-tui` forces plain unconditionally.

## Container encapsulation

Any task can be routed through a container runtime (`docker` or `podman`) by declaring a
`container { }` block in the workflow. It works at four scopes — **workflow**, **node**,
**matrix cell**, **task** — merged innermost-wins with the same precedence chain as
`env`. No CLI flags: every knob lives in the workflow, so plans stay self-describing and
CI scripts don't grow a new set of toggles.

```hocon
version = "0.0"

container {
  image   = "alpine:latest"                # default for every task in this file
  workdir = "/work"                        # bind-mount target + container cwd
  user    = "host"                         # host (default) | root | "UID:GID"
  mount   = true                           # bind host CWD at workdir (default true)
  runtime {                                # pick exactly one variant with its extras
    docker { extra_args = [ "--network=host" ] }
  }
}

nodes {
  host_only {
    container { disabled = true }          # opt back out
    tasks = [ { script = "uname -s" } ]
  }

  build {
    tasks = [ { script = "cargo build --release" } ]    # uses alpine default
  }

  cross_build {
    matrix {
      parallel = true
      dimensions = [[
        { env { vars { T = "linux"  } } },                         # alpine (default)
        { env { vars { T = "debian" } },
          container { image = "debian:bookworm-slim" } }           # override this cell
      ]]
    }
    tasks = [ { script = "cat /etc/os-release | head -1 && echo $T" } ]
  }
}
```

Running this: `alpine:latest` for `build`, host for `host_only`, and a mix of alpine +
debian for `cross_build`'s two matrix cells.

### Runtime selection

`runtime` is a nested enum: `runtime { docker { … } }` or `runtime { podman { … } }`,
each carrying the runtime's own `extra_args`. When no scope declares it, the default is
docker with no extras. There's no auto-detection — asking for a runtime that isn't on
PATH fails the task cleanly. `extra_args` accumulates across merge levels only while
consecutive levels keep the same variant; switching (docker → podman or vice versa)
resets.

```hocon
container {
  image = "rust:1.94"
  # Pick exactly one runtime variant; its extras are carried inline.
  runtime {
    docker { extra_args = [ "--network=host" ] }
  }
}
```

### Host-side semantics

- Host CWD is bind-mounted at `workdir` (default `/work`). `mount = false` disables.
- `user = "host"` maps container UID/GID to the host user — file writes into the mount
  keep correct ownership. Use `"root"` for builds that need root, or `"UID:GID"` literal.
- Task-declared `workdir` is **container-relative** (it's passed as `-w` to the runtime).
- Input streaming works through `docker run -i`: consumers declared via `inputs` receive
  their producers' JSON blob on stdin just like the host path.
- `lint` flags `container` blocks with fields set but no `image` resolved, and warns when
  the selected runtime's `extra_args` include flags neomake already sets (`-i`, `--rm`,
  `-w`, `-v`, `-t`).

### `multiplex`

The `multiplex` subcommand is a host-only runner; containers are a workflow feature.

## Native multiplexer

`neomake multiplex` is self-contained — no external multiplexer binary or crate. Architecture:

- Each command becomes one tokio task in a `JoinSet`, bounded by a tokio `Semaphore`
  (`--parallelism`). Children are `tokio::process::Command` with piped stdout/stderr read
  line-by-line and emitted as `MuxEvent`s on an mpsc channel.
- On a terminal, a ratatui TUI renders the same two-pane layout as `execute`: task tree on
  the left (status glyph + command snippet + duration), log pane on the right (stderr
  highlighted in red, follow mode, page/top/bottom scrolling, help overlay). `q` cancels
  in-flight children and exits; `Ctrl+C` cancels immediately.
- Off-terminal (piped, CI, `--no-tui`), a plain line-oriented driver prefixes each line
  with `[id]` and summarises each task on completion.
- `--stdout` emits a deterministic JSON blob with per-task stdout and RFC 3339 timestamps —
  works in both drivers.

## Watch

`neomake watch` fires commands on filesystem events that match a regex. The regex is applied
to `<event_kind>|<relative_path>`; known event kinds include:

```
access/{any,read,open/*,close/*,other}
created/{any,file,folder,other}
modified/{any,other,data/*,metadata/*,name/*}
removed/{any,file,folder,other}
any | other
```

Example events seen during development:
- `modified/data/content|src/main.rs`
- `created/file|src/module.rs`
- `created/folder|src/db`

## Why

Existing task runners (`make`, `Earthly`, `pyinvoke`, …) each miss some combination of
features neomake bakes in by default: a clean DAG, typed multi-dimensional matrix
invocations, scoped environment composition, reviewable plans, bounded parallelism, and a
native command multiplexer. The goal is a GitLab-pipeline-style execution model available
locally, as a single binary, with no daemon.

## Development

Build and test:

```bash
cargo build
cargo build --release
cargo test
cargo +nightly fmt
```

Dogfood: neomake's own release pipeline is a neomake workflow. For example:

```bash
neomake plan -n release:github -a version="$VERSION" -a signer="$SIGNER" | neomake x
neomake plan -n release:cratesio -a version="$VERSION" -a signer="$SIGNER" | neomake x
```

Tools required for the release flow:
- Rust toolchain (`cargo`, `rustc`, `rustup`)
- [`cargo-zigbuild`](https://github.com/rust-cross/cargo-zigbuild) and [Zig](https://ziglang.org/) — cross-compilation
- [`gh`](https://cli.github.com/) — GitHub releases
- `gpg` — asset signing
- `tar` — tarball creation
