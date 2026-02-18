# Hecate Plugin Directory Convention

## Overview

`~/.hecate/` is the root directory for all Hecate components on a host.
It contains three categories of entries: daemon namespaces, frontend
namespaces, and infrastructure repositories.

The primary daemon (`hecate-daemon`) is the authority on directory layout
and acts as the plugin registry.

## The Hecate App Model

Every Hecate app is a **pair**: a daemon (`*d`) and a frontend (`*w`).

| Component | Role | Runtime | Deployed as |
|-----------|------|---------|-------------|
| `hecate-daemon` | Primary daemon, plugin registry | Erlang/OTP | k3s DaemonSet |
| `hecate-web` | Native desktop shell, renders all frontends | Tauri v2 (Rust + SvelteKit) | Install script |
| `hecate-traderd` | Trading plugin daemon | Erlang/OTP | k3s DaemonSet |
| `hecate-traderw` | Trading plugin frontend | SvelteKit | k3s DaemonSet |
| `hecate-marthad` | DevOps AI agent daemon | Erlang/OTP | k3s DaemonSet |
| `hecate-marthaw` | DevOps AI agent frontend | SvelteKit | k3s DaemonSet |

### Communication Rules

1. **Frontend to its daemon** -- Unix domain sockets only. No TCP/HTTP.
   Socket files live in `~/.hecate/{daemon}/sockets/` (hostPath shared
   between host and pods).
2. **Daemon to daemon** -- BEAM-native clustering (pg process groups,
   `erlang:send/2`, monitors). Zero-config within k3s. No APIs needed.
3. **Frontend to frontend** -- NEVER. All inter-app data flows through
   daemons via BEAM clustering.
4. **hecate-web to plugin frontends** -- WebView embed. hecate-web loads
   plugin frontends as embedded webviews. The data channel from that
   webview to the plugin daemon goes via Unix socket.

### Architecture Diagram

See `assets/hecate-app-architecture.svg` for the full visual reference.

## Directory Structure

```
~/.hecate/                                  # Root for all Hecate components
  #
  # --- Infrastructure ---
  #
  gitops/                                   # Local GitOps repo (Flux source)
    hecate-daemon/                          # DaemonSet manifests
    hecate-traderd/                         # Plugin daemon manifests
    hecate-traderw/                         # Plugin frontend manifests
    hecate-marthad/                         # AI agent daemon manifests
    hecate-marthaw/                         # AI agent frontend manifests
    ...

  #
  # --- Daemon Namespaces (standard structure) ---
  #
  hecate-daemon/                            # Primary daemon (always present)
    sqlite/                                 # SQLite read-model databases
    reckon-db/                              # ReckonDB (Khepri/Ra) event store data
    sockets/                                # Unix domain sockets
      api.sock                              # Well-known bootstrap socket
    run/                                    # PID and state files
      daemon.pid
      daemon.state
    connectors/                             # Connector socket files

  hecate-traderd/                           # Trading plugin daemon
    sqlite/
    reckon-db/
    sockets/
      api.sock
    run/
      daemon.pid
      daemon.state
    connectors/

  hecate-marthad/                           # DevOps AI agent daemon
    sqlite/
    reckon-db/
    sockets/
      api.sock
    run/
      daemon.pid
      daemon.state
    connectors/
    hecate-agents/                           # Cloned knowledge base (AI instructions)

  #
  # --- Frontend Namespaces (lighter structure) ---
  #
  hecate-traderw/                           # Trading plugin frontend
    sockets/                                # Socket to its daemon
    run/

  hecate-marthaw/                           # DevOps AI agent frontend
    sockets/
    run/
```

## Three Categories

### 1. Daemon Namespaces

Every Erlang daemon gets the full standard structure:

| Subdirectory | Purpose |
|-------------|---------|
| `sqlite/` | SQLite read-model databases |
| `reckon-db/` | ReckonDB (Khepri/Ra) event store data |
| `sockets/` | Unix domain sockets (including `api.sock`) |
| `run/` | PID files, state files |
| `connectors/` | Connector socket files |

Daemons that include an AI agent (like `hecate-marthad`) may also have
a cloned knowledge base directory (e.g. `hecate-agents/`).

### 2. Frontend Namespaces

Frontends have a lighter structure -- they don't need event stores or
SQLite databases. They primarily need a socket to communicate with their
daemon.

### 3. Infrastructure

Non-daemon directories that serve the platform:

| Directory | Purpose |
|-----------|---------|
| `gitops/` | Local GitOps repository. Flux watches this for all k3s deployments. |

Infrastructure directories do NOT follow the daemon namespace structure.
They are plain git repos or configuration directories.

## Rules

1. **One directory per component** -- `~/.hecate/{component-name}/`
2. **Consistent subdirectories for daemons** -- every daemon uses the same
   internal layout (`sqlite/`, `reckon-db/`, `sockets/`, `run/`, `connectors/`)
3. **Names are unique** -- lowercase, hyphenated (e.g. `hecate-traderd`)
4. **Naming convention** -- daemons end in `d`, frontends end in `w`
5. **Well-known bootstrap socket** -- `~/.hecate/hecate-daemon/sockets/api.sock`
   is the ONE path all plugin daemons use to find the primary daemon
6. **hecate-daemon is the authority** -- it creates namespace directories
   and tracks registered plugins
7. **All deployments via gitops** -- `~/.hecate/gitops/` is the single
   source of truth for k3s deployments (except `hecate-web` which uses
   the install script)
8. **Multi-user** -- `~` resolves per-user, so each user gets their own
   `~/.hecate/` tree

## Bootstrap Flow

A plugin daemon starts by connecting to the well-known socket:

```
1. Plugin daemon starts (deployed via gitops as DaemonSet)
2. Connects to ~/.hecate/hecate-daemon/sockets/api.sock
3. POST /api/plugins/register { "name": "hecate-traderd" }
4. hecate-daemon creates ~/.hecate/hecate-traderd/{sqlite,reckon-db,...}
5. Returns the assigned paths to the plugin
6. Plugin daemon uses those paths for all its data
7. Plugin daemon joins BEAM cluster for inter-daemon communication
```

## Plugin Registration API

### Register a plugin daemon

```
POST /api/plugins/register
Content-Type: application/json

{
  "name": "hecate-traderd",
  "version": "0.1.0",
  "description": "Trading daemon for Hecate"
}

Response 201:
{
  "name": "hecate-traderd",
  "base_dir": "/home/user/.hecate/hecate-traderd",
  "paths": {
    "sqlite":     "/home/user/.hecate/hecate-traderd/sqlite",
    "reckon_db":  "/home/user/.hecate/hecate-traderd/reckon-db",
    "sockets":    "/home/user/.hecate/hecate-traderd/sockets",
    "run":        "/home/user/.hecate/hecate-traderd/run",
    "connectors": "/home/user/.hecate/hecate-traderd/connectors"
  }
}
```

### List registered plugins

```
GET /api/plugins

Response 200:
[
  {
    "name": "hecate-traderd",
    "version": "0.1.0",
    "socket": "/home/user/.hecate/hecate-traderd/sockets/api.sock",
    "status": "running"
  }
]
```

### Deregister a plugin

```
DELETE /api/plugins/hecate-traderd

Response 200:
{ "ok": true }
```

Note: deregistration does NOT delete the plugin's data directory. Data
cleanup is a separate, explicit operation.

## Why This Design

| Concern | Solution |
|---------|----------|
| Plugin daemons don't hardcode conventions | They ask hecate-daemon for paths |
| Directory layout can evolve | Only hecate-daemon needs updating |
| hecate-daemon tracks what's installed | Plugin registry as source of truth |
| No conflicts between daemons | Each gets its own namespace |
| Discovery | Plugins find each other through hecate-daemon |
| Security | Unix sockets, no TCP exposure for frontend-daemon |
| Backend communication | BEAM clustering, no APIs needed |
| Deployment | Local gitops, consistent for all components |
| Multi-user | Per-user home directory isolation |

## Implementation

The directory layout is enforced by `shared_paths.erl` in hecate-daemon:

- `shared_paths:base_dir/0` -- returns `~/.hecate/hecate-daemon`
- `shared_paths:sqlite_path/1` -- returns `base_dir/sqlite/Name`
- `shared_paths:reckon_path/1` -- returns `base_dir/reckon-db/Name`
- `shared_paths:socket_path/1` -- returns `base_dir/sockets/Name`
- `shared_paths:run_path/1` -- returns `base_dir/run/Name`
- `shared_paths:connectors_dir/0` -- returns `base_dir/connectors`
- `shared_paths:ensure_layout/0` -- creates all subdirectories

The base directory is configured via `{hecate, [{data_dir, "~/.hecate/hecate-daemon"}]}`.

## Consumers

| Component | What it needs | How it finds it |
|-----------|---------------|-----------------|
| hecate-daemon (Erlang) | Everything | `shared_paths` module |
| hecate-web (Tauri/Rust) | Socket path | Hardcoded well-known path |
| Shell scripts | Socket + PID paths | `$HOME/.hecate/hecate-daemon/sockets/api.sock` |
| Plugin daemons | Their own namespace | Plugin registration API |
| Plugin frontends | Daemon socket path | Convention: `~/.hecate/{daemon}/sockets/api.sock` |
| k3s deployments | Socket path + data dirs | hostPath volumes to `~/.hecate/` |
| Flux/GitOps | Manifests | `~/.hecate/gitops/` |
| AI agents (Martha) | Knowledge base | `~/.hecate/hecate-marthad/hecate-agents/` |
