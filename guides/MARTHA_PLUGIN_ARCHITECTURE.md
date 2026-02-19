# Martha Plugin Architecture

_How Martha implements the ALC as a hecate-web plugin._

**Date:** 2026-02-19
**Status:** Active

---

## What Is Martha?

Martha is the Hecate plugin that implements the Application Lifecycle (ALC). She guides users through venture setup, division discovery, and the eight-process development cycle -- from design through rescue.

Martha follows the standard Hecate plugin model: a daemon (`hecate-marthad`) communicating via Unix socket, a frontend (`hecate-marthaw`) compiled to an ES module, and CLI subcommands.

**Martha is to the ALC what QuickBooks is to accounting** -- the tool that implements the discipline.

---

## Repository Structure

```
hecate-martha/
  Dockerfile                         # Unified build: frontend + daemon

  hecate-marthad/                    # Erlang/OTP daemon
    rebar.config
    config/
      sys.config
      vm.args
    apps/
      guide_venture_lifecycle/       # CMD: venture + discovery lifecycle
      guide_division_alc/            # CMD: division 8-process lifecycle
      query_venture_lifecycle/       # QRY+PRJ: venture read models
      query_division_alc/            # QRY+PRJ: division read models
      hecate_marthad/                # App shell: Cowboy, socket, health, manifest
    priv/static/                     # Frontend bundle (component.js)

  hecate-marthaw/                    # SvelteKit frontend
    vite.config.lib.ts               # Library build -> dist/component.js
    src/lib/
      MarthaStudio.svelte            # Root component (receives {api} prop)
      ... (vertical slices, see below)
```

---

## Runtime

```
~/.hecate/hecate-marthad/
  sqlite/            # Division/venture read models
  reckon-db/         # Event store data
  sockets/
    api.sock         # Discovered by hecate-web plugin watcher
  run/
  connectors/
  hecate-agents/     # AI knowledge base (cloned at install)
```

Socket path: `~/.hecate/hecate-marthad/sockets/api.sock`
Manifest: `GET /manifest` returns `{name: "martha", icon: ..., version: ..., description: ...}`

---

## Frontend Vertical Slices

The frontend is organized by **ALC process**, not by technical concern. Each vertical slice contains its own component, store, and types. No `components/`, `stores/`, `utils/` directories.

### Venture-Level Slices

| Slice | ALC Process | What It Does |
|-------|-------------|--------------|
| `compose_vision/` | `setup_venture` | AI-guided venture vision creation (Oracle conversation + live preview) |
| `discover_divisions/` | `discover_divisions` | Big Picture Event Storming (7 phases: storm, stack, groom, cluster, name, map, promote) |

### Division ALC Slices (8 processes)

| Slice | ALC Process | What It Does |
|-------|-------------|--------------|
| `design_division/` | Design | Design-level event storming, desk cards, aggregate grouping |
| `plan_division/` | Planning | Desk inventory, dependency mapping, sprint sequencing |
| `craft_division/` | Crafting | Code generation, module scaffolding |
| `refactor_division/` | Refactoring | Structural improvement analysis and execution |
| `debug_division/` | Debugging | Test suites, acceptance criteria, defect diagnosis |
| `deploy_division/` | Deployment | Release management, staged rollout |
| `monitor_division/` | Monitoring | Health checks, SLA tracking, anomaly detection |
| `rescue_division/` | Rescue | Incident response, diagnosis, escalation |

### Orchestration + Shared

| Slice | Purpose |
|-------|---------|
| `guide_venture/` | Venture header, division navigation, phase progress, lifecycle controls |
| `shared/` | TaskCard, AIAssistPanel, EventStreamViewer, ModelSelector |

### Slice Anatomy

Every slice follows the same pattern:

```
{slice_name}/
  {SliceName}.svelte     # Main view component
  {slice_name}.ts        # Stores + API calls (reactive state)
  types.ts               # Types scoped to this slice
```

Example:

```
discover_divisions/
  DiscoverDivisions.svelte    # Big Picture board UI
  discover_divisions.ts       # Storm state, sticky CRUD, cluster ops
  types.ts                    # StickyNote, EventCluster, FactArrow, StormPhase
```

### Why Vertical Slicing?

The current hecate-web DevOps studio uses horizontal layers:

```
components/devops/     # 13 components grouped by tech
stores/devops.ts       # 1242-line god-store for ALL state
types.ts               # All types in one file
```

This means understanding "how does event storming work?" requires reading 3+ files scattered across the tree. With vertical slicing:

- **To understand event storming** -- read `discover_divisions/` (one directory)
- **To understand deployment** -- read `deploy_division/` (one directory)
- **To add monitoring features** -- edit `monitor_division/` (one directory)

The directory name screams what it does. All related code lives together.

---

## Daemon Architecture

The daemon is an Erlang/OTP umbrella with apps mirroring the ALC:

### CMD Apps (Process-Centric)

| App | Manages |
|-----|---------|
| `guide_venture_lifecycle` | Venture initiation, vision, Big Picture storm, division discovery |
| `guide_division_alc` | All 8 division processes: design through rescue |

### QRY+PRJ Apps (Data-Centric)

| App | Read Models |
|-----|-------------|
| `query_venture_lifecycle` | Ventures, storm sessions, stickies, clusters, discovered divisions |
| `query_division_alc` | Divisions, aggregates, events, desks, deps, modules, tests, releases, incidents |

### API Shell

| Module | Purpose |
|--------|---------|
| `hecate_marthad_app` | Starts Cowboy on Unix socket, ensures directory layout |
| `hecate_marthad_sup` | Top-level supervisor |
| `marthad_paths` | Path helpers for `~/.hecate/hecate-marthad/` |
| `marthad_health_api` | `GET /health` |
| `marthad_manifest_api` | `GET /manifest` |
| `marthad_api_handler` | Domain API routes (venture/division commands + queries) |

---

## Cross-Daemon Communication

Martha's frontend component receives an `api` prop from hecate-web that routes to the Martha daemon socket. But Martha also needs capabilities from the main hecate-daemon:

- **LLM streaming** -- AI-assisted vision, event storming, domain expertise
- **Agent prompts** -- Loaded from hecate-agents knowledge base

This flows through BEAM clustering (same cookie, same network):

```
MarthaStudio.svelte
  |  api.post('/chat', {model, messages})
  v
hecate-marthad (Cowboy)
  |  erlang:send / pg
  v
hecate-daemon (serve_llm)
  |  LLM provider API
  v
Response streams back through the chain
```

Martha daemon proxies LLM requests to hecate-daemon. The frontend never talks to hecate-daemon directly -- all requests go through the Martha socket.

---

## Migration from hecate-web

Martha is being extracted from hecate-web's built-in DevOps studio. The extraction involves:

1. **Frontend**: Move 13 components + 1 store + types from `hecate-web/src/lib/` to `hecate-marthaw/src/lib/`, restructured as vertical slices
2. **Backend**: Move 4 Erlang apps from `hecate-daemon/apps/` to `hecate-marthad/apps/`
3. **API routes**: Extract venture/division routes from `hecate_api_routes.erl` to Martha's own API handler
4. **Decouple**: Remove Martha-specific error codes from shared `api.ts`, Martha-specific types from shared `types.ts`, Martha-specific StatusBar logic

After extraction, Martha registers as `{id: "martha", path: "/martha"}` in the plugin system, discovered automatically via the socket convention.

---

## Reference Implementation

The Trader plugin (`hecate-social/hecate-trader/`) is the reference for:
- Daemon structure (Cowboy on Unix socket, health/manifest endpoints)
- Frontend build (Vite library mode, externalized Svelte runtime)
- Plugin discovery (directory naming, socket watching)
- Component loading (ES module via blob URL, `api` prop injection)

Martha follows the same pattern but is significantly more complex (event sourcing, AI integration, 122 Erlang source files vs Trader's handful).
