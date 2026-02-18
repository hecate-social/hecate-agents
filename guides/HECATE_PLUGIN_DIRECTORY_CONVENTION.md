# Hecate Plugin Directory Convention

## Overview

Multiple daemons coexist under `~/.hecate/`. Each daemon gets its own
**namespace directory** with a consistent internal structure. The primary
daemon (`hecate-daemon`) is the authority on directory layout and acts as
the plugin registry.

## Directory Structure

```
~/.hecate/                              # Root for all Hecate daemons
  hecate-daemon/                        # Primary daemon (always present)
    sqlite/                             # SQLite read-model databases
    reckon-db/                          # ReckonDB (Khepri/Ra) event store data
    sockets/                            # Unix domain sockets
      api.sock                          # Main API socket (well-known path)
    run/                                # PID and state files
      daemon.pid
      daemon.state
    connectors/                         # Connector socket files

  hecate-traderd/                       # Example plugin daemon
    sqlite/
    reckon-db/
    sockets/
      api.sock
    run/
      daemon.pid
      daemon.state
    connectors/

  third-party-daemon/                   # Third-party plugin
    ...same structure...
```

## Rules

1. **One directory per daemon** -- `~/.hecate/{daemon-name}/`
2. **Consistent subdirectories** -- every daemon uses the same internal
   layout (`sqlite/`, `reckon-db/`, `sockets/`, `run/`, `connectors/`)
3. **Daemon names are unique** -- lowercase, hyphenated (e.g. `hecate-traderd`)
4. **Well-known bootstrap socket** -- `~/.hecate/hecate-daemon/sockets/api.sock`
   is the ONE path all plugin daemons use to find the primary daemon
5. **hecate-daemon is the authority** -- it creates namespace directories
   and tracks registered plugins

## Bootstrap Flow

A plugin daemon starts by connecting to the well-known socket:

```
1. Plugin daemon starts
2. Connects to ~/.hecate/hecate-daemon/sockets/api.sock
3. POST /api/plugins/register { "name": "hecate-traderd" }
4. hecate-daemon creates ~/.hecate/hecate-traderd/{sqlite,reckon-db,...}
5. Returns the assigned paths to the plugin
6. Plugin daemon uses those paths for all its data
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
| k3s deployments | Socket path | `HECATE_SOCKET_PATH` env var override |
