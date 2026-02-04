# CARTWHEEL.md — The Canonical Architecture

*DisComCo in practice. This is the blueprint.*

---

## Overview

**Cartwheel** has three sequences and four mesh components:

| Sequence | Purpose |
|----------|---------|
| **CMD** | Command/Write side — receives intents, produces events |
| **PRJ** | Projection — subscribes to events, updates read models |
| **QRY** | Query/Read side — serves queries from read models |

| Mesh Component | Direction | Purpose |
|----------------|-----------|---------|
| **RESPONDER** | Mesh → Domain | Receives HOPEs, translates to Commands |
| **EMITTER** | Domain → Mesh | Subscribes to Events, publishes FACTs |
| **REQUESTER** | Domain → Mesh | Sends HOPEs, awaits responses |
| **LISTENER** | Mesh → Domain | Receives FACTs, triggers local processing |

---

## CMD Domain Structure

A CMD domain app is composed of **spokes** (vertical slices) + optional **shared infrastructure**.

```
apps/manage_capabilities/
├── src/
│   ├── manage_capabilities_app.erl
│   ├── manage_capabilities_sup.erl       # Starts spokes + shared infra ONLY
│   │
│   ├── (shared infrastructure - optional)
│   │   ├── manage_capabilities_store.erl # ReckonDB instance
│   │   └── manage_capabilities_exchange.erl # Internal pub/sub
│   │
│   ├── announce_capability/              # SPOKE (vertical slice)
│   │   ├── announce_capability_spoke_sup.erl
│   │   ├── announce_capability_v1.erl
│   │   ├── capability_announced_v1.erl
│   │   ├── maybe_announce_capability.erl
│   │   ├── announce_capability_responder_v1.erl
│   │   └── capability_announced_v1_to_mesh.erl
│   │
│   ├── update_capability/                # SPOKE
│   │   └── ...
│   │
│   └── retract_capability/               # SPOKE
│       └── ...
│
└── rebar.config                          # Include src_dirs for spokes
```

---

## CMD Spoke Contents

Every CMD spoke contains:

| File | Type | Purpose |
|------|------|---------|
| `*_spoke_sup.erl` | Supervisor | Supervises all workers in this spoke |
| `*_v1.erl` | Record | Command struct (`new/N`, `to_map/1`, `from_map/1`) |
| `*_v1.erl` | Record | Event struct (what happened) |
| `maybe_*.erl` | Handler | Validates command, dispatches via evoq |
| `*_responder_v1.erl` | gen_server | HOPE → Command translator (mesh inbound) |
| `*_to_mesh.erl` | gen_server | Event → FACT emitter (mesh outbound) |

**Optional:**
| File | Type | Purpose |
|------|------|---------|
| `on_{event}_maybe_*.erl` | Policy/PM | Cross-domain integration trigger |

---

## CMD Spoke Flow

### Inbound (Mesh → Domain)

```
HOPE (announce_capability_hope_v1)
    ↓
Responder (announce_capability_responder_v1)
    ↓ translates
Command (announce_capability_v1)
    ↓
Handler (maybe_announce_capability)
    ↓ validates, dispatches
Event (capability_announced_v1)
    ↓
Store (ReckonDB)
```

### Outbound (Domain → Mesh)

```
Event (capability_announced_v1)
    ↓
Emitter (capability_announced_v1_to_mesh)
    ↓ subscribes to store, transforms
FACT (published to mesh topic)
```

### Cross-Domain (via Process Manager)

```
Event from Domain A (llm_model_detected_v1)
    ↓
Policy/PM (on_llm_model_detected_maybe_announce_capability)
    ↓ subscribes to Domain A, dispatches to Domain B
Command to Domain B (announce_capability_v1)
    ↓
Normal CMD flow in Domain B
```

---

## Domain Supervisor Pattern

**Domain supervisor ONLY starts:**
1. Shared infrastructure (store, exchange)
2. Spoke supervisors

**Domain supervisor NEVER directly supervises workers.**

```erlang
%% manage_capabilities_sup.erl
init([]) ->
    Children = [
        %% Shared infra
        #{id => manage_capabilities_store,
          start => {manage_capabilities_store, start_link, []},
          type => worker},
        
        %% Spokes (supervisors, not workers!)
        #{id => announce_capability_spoke_sup,
          start => {announce_capability_spoke_sup, start_link, []},
          type => supervisor},
        #{id => update_capability_spoke_sup,
          start => {update_capability_spoke_sup, start_link, []},
          type => supervisor},
        #{id => retract_capability_spoke_sup,
          start => {retract_capability_spoke_sup, start_link, []},
          type => supervisor}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.
```

---

## Spoke Supervisor Pattern

**Spoke supervisor starts all workers for that spoke.**

```erlang
%% announce_capability_spoke_sup.erl
init([]) ->
    Children = [
        #{id => announce_capability_responder_v1,
          start => {announce_capability_responder_v1, start_link, []},
          type => worker},
        #{id => capability_announced_v1_to_mesh,
          start => {capability_announced_v1_to_mesh, start_link, []},
          type => worker}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.
```

---

## Naming Conventions

| Component | Pattern | Example |
|-----------|---------|---------|
| Spoke directory | `{verb}_{noun}/` | `announce_capability/` |
| Spoke supervisor | `{verb}_{noun}_spoke_sup.erl` | `announce_capability_spoke_sup.erl` |
| Command | `{verb}_{noun}_v1.erl` | `announce_capability_v1.erl` |
| Event | `{noun}_{past_verb}_v1.erl` | `capability_announced_v1.erl` |
| Handler | `maybe_{verb}_{noun}.erl` | `maybe_announce_capability.erl` |
| Responder | `{verb}_{noun}_responder_v1.erl` | `announce_capability_responder_v1.erl` |
| Emitter | `{event}_to_mesh.erl` | `capability_announced_v1_to_mesh.erl` |
| Policy/PM | `on_{event}_maybe_{verb}_{noun}.erl` | `on_llm_model_detected_maybe_announce_capability.erl` |
| HOPE struct | `{verb}_{noun}_hope_v1.erl` | `announce_capability_hope_v1.erl` |

---

## PRJ Domain Structure

Projections subscribe to events and update read models (SQLite).

```
apps/query_capabilities/
├── src/
│   ├── query_capabilities_app.erl
│   ├── query_capabilities_sup.erl
│   ├── query_capabilities_store.erl      # SQLite read model
│   │
│   ├── capability_announced_v1_to_capabilities/  # PRJ spoke
│   │   ├── capability_announced_v1_to_capabilities_sup.erl
│   │   └── capability_announced_v1_to_capabilities.erl
│   │
│   └── capability_retracted_v1_to_capabilities/  # PRJ spoke
│       └── ...
```

---

## QRY Domain Structure

Queries read from projections. Simple functions, no events.

```
apps/query_capabilities/
├── src/
│   ├── find_capability/
│   │   └── find_capability.erl           # execute/1 → query store
│   │
│   └── list_capabilities/
│       └── list_capabilities.erl         # execute/1 → query store
```

---

## Code Generation

This architecture is **strict enough for deterministic code generation**.

Given:
- Domain name: `manage_capabilities`
- Spoke name: `announce_capability`
- Event name: `capability_announced`

A generator can produce:
- [ ] `announce_capability_spoke_sup.erl`
- [ ] `announce_capability_v1.erl`
- [ ] `capability_announced_v1.erl`
- [ ] `maybe_announce_capability.erl`
- [ ] `announce_capability_responder_v1.erl`
- [ ] `capability_announced_v1_to_mesh.erl`
- [ ] Update `manage_capabilities_sup.erl` to include spoke
- [ ] Update `rebar.config` src_dirs

**No AI needed.** Templates + naming conventions = complete spoke.

---

*The wheel turns. The spokes hold.* 🔥🗝️🔥
