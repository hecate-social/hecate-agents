---
title: "FAQ: How Do I Run macula-cli?"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Run macula-cli?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

Sourced from `macula-io/macula-cli`'s own README, `guides/HOWTO.md`, and a
live run of the actually-installed binary (`macula-cli 0.5.0` at the time of
writing) — every command's help text below is the real `-h` output, not
paraphrased.

---

## 1. Install

**Linux / macOS:**

```bash
curl -fsSL https://raw.githubusercontent.com/macula-io/macula-cli/master/install.sh | bash
```

**Windows (PowerShell):**

```powershell
irm https://raw.githubusercontent.com/macula-io/macula-cli/master/install.ps1 | iex
```

Both pull the release archive matching your OS/arch from GitHub Releases,
verify it against the release's own `checksums.txt`, and install to
`$HOME/.local/bin` (Linux/macOS) or `%LOCALAPPDATA%\macula-cli` (Windows) —
override with `MACULA_CLI_INSTALL_DIR`.

Already have Go? `go install github.com/macula-io/macula-cli/cmd/macula-cli@latest`
works too.

```bash
macula-cli connect station-de-frankfurt.macula.io
```

To remove it: same repo path, `uninstall.sh`/`uninstall.ps1` instead of
`install`. Leaves the persisted identity alone by default (`--purge`/`-Purge`
to remove that too).

---

## 2. Does it support `--help`?

Yes, at every level. Top-level:

```
$ macula-cli --help
macula-cli — test, monitor, and diagnose the Macula mesh

Usage:
  macula-cli connect <host[:port]>                    staged handshake diagnostic (DNS, QUIC, HELLO)
  macula-cli call <host[:port]> <procedure>            unary RPC call
  macula-cli call -via-daemon <procedure>              same, routed through a running daemon
  macula-cli serve <host[:port]> <procedure>           advertise, answer one inbound CALL, exit
  macula-cli serve -daemon <procedure>                 register with a running daemon, answer many calls
  macula-cli pubsub watch <host[:port]> <topic>        subscribe and print events as they arrive
  macula-cli pubsub watch -daemon <topic>              tap a daemon's own subscription
  macula-cli pubsub publish <host[:port]> <topic>      publish one event to a topic
  macula-cli pubsub subscribe <topic>                  daemon-only: start a durable subscription
  macula-cli pubsub unsubscribe <topic>                daemon-only: end a durable subscription
  macula-cli stream probe                              cross-station streaming round trip
  macula-cli content probe <host[:port]>               content put/get/verify round trip
  macula-cli content put <host[:port]> <file>          upload a file, print its MCID
  macula-cli content get <host[:port]> <mcid>          download by MCID
  macula-cli dht find-record <host[:port]> <key-hex>   fetch one DHT record by storage key
  macula-cli dht find-records <host[:port]> <key-hex>  fetch every record at a storage key
  macula-cli dht find-records-by-type <host[:port]> <type>  list every record of a type (discovery)
  macula-cli identity                                  print the local identity's node ID
  macula-cli identity sign --procedure <name>          sign a {node_id, timestamp, procedure} ownership proof
  macula-cli ucan mint <issuer> <audience>              mint a UCAN token, signed by the local identity
  macula-cli ucan inspect <token-file>                  decode a UCAN token's claims (no signature check)
  macula-cli daemon start <host[:port]>                 hold one Session open, serve registered procedures
  macula-cli daemon status                              show what a running daemon is serving/subscribed to
  macula-cli daemon stop                                ask a running daemon to shut down

Run "macula-cli <command> -h" for a command's own flags.
```

Every subcommand takes `-h`/`--help` too (e.g. `macula-cli call -h`,
`macula-cli serve -h`, `macula-cli daemon start -h`) and prints its full flag
reference plus usage notes — the exact same text that's excerpted in the
sections below.

⚠ **Gotcha (documented in the CLI's own README):** Go's `flag` package
requires flags before positional arguments — `macula-cli call -json
station.example.com my.proc` works, `macula-cli call station.example.com
my.proc -json` does not. And Macula's wire protocol has **no `bool` type**:
a JSON boolean in `-args` is rejected outright, not silently coerced — use
`0`/`1`.

---

## 3. One-shot / scripted mode

Every command except `daemon start` is one-shot by design: connect, do the
one thing, exit. This is the natural fit for shell scripting and CI.

**All flags before the positional `<host>`/`<procedure>`/`<topic>`
arguments** — the §2 gotcha applies to every example below, not just as an
abstract warning; a flag placed after the positional args is silently
treated as unparsed and the command prints its own usage text instead of
running, verified live while writing this page.

```bash
# Call a procedure and print its result
macula-cli call -args '"hello"' station-de-frankfurt.macula.io:4433 io.macula.echo

# Publish one event
macula-cli pubsub publish -args '{"n":1}' station-de-frankfurt.macula.io:4433 my.topic

# Watch a topic for up to 30s or 5 events, whichever comes first
macula-cli pubsub watch -duration 30s -count 5 station-de-frankfurt.macula.io:4433 my.topic

# Serve exactly one call, then exit
macula-cli serve -echo station-de-frankfurt.macula.io:4433 my.echo
```

`-json` on any command emits a machine-parseable JSON result envelope
instead of human-readable text — the shape a script or another program
should read, rather than scraping the text output.

**There is no interactive REPL/shell mode.** The closest thing to
"interactive, held-open state" is daemon mode (§5) — a foreground process
plus other one-shot invocations talking to it over a control socket, not a
prompt you type commands into directly.

---

## 4. Config: just an identity file, no settings file

macula-cli has no `.macularc`/config.yaml/config.toml of any kind — every
option is a CLI flag, with sensible defaults. The one thing it persists is
the **identity seed**, via `-identity <path>` (default: your OS's standard
user config directory, `os.UserConfigDir()` — `$XDG_CONFIG_HOME` or
`~/.config` on Linux, `~/Library/Application Support` on macOS, `%AppData%`
on Windows). Reusing the same identity across invocations is what lets a
served procedure or a daemon-registered one keep the same node ID across
restarts.

---

## 5. Daemon mode + hooks

**Daemon mode** is for a long-lived server: one `macula-cli daemon start`
process holds a station connection open, and other `macula-cli` invocations
control it over a local Unix domain socket — the same shape as `ssh-agent`
or `dockerd`, not a second product.

```bash
# Start the daemon (foreground -- pair with a process supervisor for
# unattended use; Ctrl-C/SIGTERM/"daemon stop" all stop it cleanly).
macula-cli daemon start station-de-frankfurt.macula.io:4433 &

# Register a procedure -- answers as many calls as arrive, not just one.
macula-cli serve -daemon -reply '{"pong":1}' my.echo

# Or -exec: a real, per-call computed reply instead of a fixed one --
# this is the hook mechanism (see below).
macula-cli serve -daemon -exec './double.sh' my.double

# From anywhere else: ordinary "call" reaches it exactly like any other
# advertised procedure -- the daemon is invisible to callers. Or route
# through the daemon's own connection instead of dialing fresh:
macula-cli call station-de-frankfurt.macula.io:4433 my.echo
macula-cli call -via-daemon my.echo

# A subscription outlives the command that created it.
macula-cli pubsub subscribe my.topic
macula-cli pubsub watch -daemon my.topic &
macula-cli pubsub unsubscribe my.topic

# Inspect or stop it.
macula-cli daemon status
macula-cli serve -daemon -stop my.echo
macula-cli daemon stop
```

**Hooks are `-exec`.** `serve -daemon -exec '<shell command>'` runs that
command once per inbound CALL — the call's payload arrives as one JSON
document on the command's stdin, and its entire stdout is parsed as the
reply (empty stdout replies `null`). A non-zero exit, a timeout
(`-exec-timeout`, default 10s), or invalid JSON on stdout all become a
normal ERROR reply to the *caller* — none of the three crashes the daemon
process or corrupts any other procedure's registration. `sh -c` on
Linux/macOS, `cmd /C` on Windows.

⚠ **But it does stall every other procedure on the same daemon while it
runs.** All procedures a daemon serves share one receive loop (a plain
sequential `ServeForever` loop in the underlying SDK) that invokes each
resolved handler synchronously — the loop can't read the next inbound
CALL for *any* procedure until the current one's `-exec` command returns
or the timeout fires. A slow hook for one procedure is a real,
budget-`(-exec-timeout)`-sized latency spike for every unrelated procedure
that daemon is serving, not just its own — the source's own comment on
the default timeout says this outright. Keep `-exec` commands fast, or
give a slow one its own dedicated daemon (`-socket-name`).

```bash
# double.sh
#!/bin/sh
python3 -c 'import json,sys; print(json.dumps(json.load(sys.stdin)["n"]*2))'
```

**Three Sessions, not one**, under the hood: one for serving/advertising
(the daemon's real, persisted identity), one ephemeral-identity Session
dedicated to `call -via-daemon`, and one shared third Session for every
`pubsub subscribe`d topic together — a single receive loop that dispatches
by topic, not one Session per topic. This is deliberate — a single-Session
build hit a real race where answering inbound calls while making an
outbound one intermittently stole the reply meant for the outbound
caller.

More than one daemon instance can run side by side via `-socket-name` (e.g.
one per identity/realm) — every daemon-aware command takes it.

---

## 6. A minimal worked example end to end

Live-tested writing this page, on the public demo fleet:

```bash
# Terminal 1: start a daemon and register a procedure backed by a script
macula-cli daemon start station-de-frankfurt.macula.io:4433 &
macula-cli serve -daemon -exec './double.sh' my.double

# Terminal 2: call it back through the SAME daemon that registered it
macula-cli call --json -via-daemon -args '{"n":21}' my.double
# -> {"ok":true,"data":{...,"payload":42,...}}  -- confirmed live

# Check what the daemon is currently serving/subscribed to
macula-cli daemon status

# Clean up
macula-cli serve -daemon -stop my.double
macula-cli daemon stop
```

**`-via-daemon` is the reliable form here, not incidental.** A *fresh*
non-daemon `call` to the same host for the same just-registered
procedure —
`macula-cli call --json -args '{"n":21}' station-de-frankfurt.macula.io:4433 my.double`
— failed consistently in testing (`unknown_next_peer`, retryable per the
error envelope), across several attempts and wait times up to 5s. Whether
that's ordinary advertise-gossip propagation lag on the shared demo fleet
or something specific to calling back into the same station a daemon is
already holding open wasn't run down further here — but `-via-daemon`
reusing the daemon's own live session sidesteps the question entirely and
is what's actually verified working. If you need a fresh, non-daemon
caller (a different process, possibly a different station) to reach a
daemon-registered procedure, budget for it not working on the first try
and don't be surprised — file it against `macula-cli`/`macula-station` if
you can reproduce it reliably enough to isolate.

---

## See also

- [`macula-io/macula-cli`'s own README](https://github.com/macula-io/macula-cli) —
  architecture, the full daemon-mode writeup with real production gotchas
  (a fixed 2026-08-31 race on long topic names, the control-socket path
  length limit)
- [`macula-io/macula-cli/guides/HOWTO.md`](https://github.com/macula-io/macula-cli/blob/master/guides/HOWTO.md) —
  the complete command/flag reference this FAQ entry summarizes, with
  example output for every command
- [FAQ: How do I run macula-mcp?](FAQ_MACULA_MCP.md) — the MCP server built on this CLI
- [FAQ: How do I join the Mesh?](FAQ_JOIN_THE_MESH.md) — running your own station, not just a client
