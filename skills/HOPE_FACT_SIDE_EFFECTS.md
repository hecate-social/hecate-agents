# Side Effects Must Follow Facts, Not Hopes

*An architectural pattern for TUI ↔ Daemon communication.*

---

## The Principle

> **The TUI is an external system. Side effects (file I/O, state changes, UI updates) must be driven by received FACTS (events), never by command acknowledgments.**

A command sent to the daemon is a **HOPE** — "I hope you'll do this." The HTTP response means "I received your hope and it looks valid." It does NOT mean the intent succeeded. Only the resulting **event** (fact) confirms that.

---

## The Anti-Pattern

```
TUI                              Daemon
────                             ──────
POST /vision/refine  ──────→    receive hope
                                process command
                                store event (maybe)
← 200 OK  ←───────────────     "received"
write VISION.md  ← WRONG!       side effect based on hope
```

What can go wrong between "received" and "event stored":
- Aggregate rejects the command (business rule violation)
- Event store write fails (disk full, replication error)
- Process crashes between accept and store
- Command is queued for async processing

The TUI wrote to disk based on a **hope acknowledgment**, not a **fact**.

---

## The Correct Pattern: Per-Fact Listeners

The TUI hosts **listeners** — one per fact type it cares about. Each listener owns its subscription AND its side effect. This is the same pattern as daemon-side pg listeners, applied to the TUI.

```
TUI                              Daemon
────                             ──────
listeners subscribe via SSE ──→  register per-fact emitters

POST /vision/refine  ──────→    receive hope
← 202 Accepted  ←─────────     "hope received"
                                process command
                                vision_refined_v1 stored
                                vision_refined_v1_to_pg
                                vision_refined_v1_to_tui
                                  ↓
on_vision_refined_v1             ← SSE fact received
  _write_vision_to_disk.go
  → scaffold VISION.md          side effect based on FACT
```

---

## Listener Architecture

### Daemon Side: Per-Fact TUI Emitters

Each fact that the TUI needs has a dedicated emitter, following the existing `_to_pg` / `_to_mesh` pattern:

```
{event}_to_pg.erl       ← internal (same BEAM VM)
{event}_to_mesh.erl     ← external (WAN, cross-daemon)
{event}_to_tui.erl      ← local (same machine, cross-process)
```

The `_to_tui` emitters:
1. Join the pg group for their fact (same as any internal listener)
2. Forward received facts to connected TUI subscribers over SSE
3. Share a single SSE connection under the hood (transport plumbing)

```erlang
%% vision_refined_v1_to_tui.erl
-module(vision_refined_v1_to_tui).
-behaviour(gen_server).

-define(GROUP, vision_refined_v1).

init([]) ->
    ok = pg:join(?GROUP, self()),
    {ok, #{}}.

handle_info({vision_refined_v1, Event}, State) ->
    %% Forward to connected TUI via shared SSE bridge
    tui_bridge:emit(vision_refined_v1, Event),
    {noreply, State}.
```

### TUI Side: Per-Fact Listeners

Each fact the TUI reacts to has a dedicated listener. The listener is a self-contained unit — it owns its subscription AND its reaction.

```go
// on_vision_refined_v1_write_vision_to_disk.go

type VisionRefinedListener struct {
    ventureRoot string
}

func (l *VisionRefinedListener) FactType() string {
    return "vision_refined_v1"
}

func (l *VisionRefinedListener) OnFact(fact Event) tea.Cmd {
    manifest := scaffold.VentureManifest{
        Name:  fact.Data["name"],
        Brief: fact.Data["brief"],
    }
    scaffold.ScaffoldVision(l.ventureRoot, manifest)
    return nil  // or return a tea.Msg to update UI
}
```

### Naming Convention

Daemon emitters:
```
{event}_to_tui.erl
```

TUI listeners:
```
on_{event}_{side_effect}.go
```

Examples:

| Daemon Emitter | TUI Listener | Side Effect |
|----------------|--------------|-------------|
| `vision_refined_v1_to_tui.erl` | `on_vision_refined_v1_write_vision_to_disk.go` | Scaffold/update VISION.md |
| `vision_submitted_v1_to_tui.erl` | `on_vision_submitted_v1_update_status.go` | Update venture status in UI |
| `venture_archived_v1_to_tui.erl` | `on_venture_archived_v1_clear_context.go` | Clear ALC context, update statusbar |

---

## Transport: SSE over Unix Socket

The transport is **shared SSE** — one persistent connection, multiplexed. But listeners don't know or care. They register for a fact type and get called.

Think of it like pg in Erlang:
- `pg:join(Group, Pid)` — one process joins one group
- Under the hood, pg uses Erlang distribution (one connection per node)
- The abstraction hides the transport

Similarly:
- `factBus.Register("vision_refined_v1", listener)` — one listener per fact
- Under the hood, one SSE connection carries all facts
- The abstraction hides the transport

### Daemon Side: tui_bridge

A shared process that manages the SSE connection to the TUI:

```erlang
%% tui_bridge.erl — shared transport plumbing
-module(tui_bridge).

%% Called by per-fact emitters (_to_tui.erl modules)
emit(FactType, Event) ->
    %% Format as SSE and send to connected TUI
    Data = json:encode(#{
        <<"fact_type">> => atom_to_binary(FactType),
        <<"data">> => Event
    }),
    %% Send to all connected TUI processes (SSE handlers)
    pg:broadcast(tui_connections, {tui_fact, Data}).
```

### TUI Side: FactBus

A registry that maps fact types to listeners:

```go
// factbus.go — transport plumbing
type FactBus struct {
    listeners map[string]FactListener
    client    *client.Client
}

func (fb *FactBus) Register(factType string, listener FactListener) {
    fb.listeners[factType] = listener
}

// dispatch is called when an SSE event arrives
func (fb *FactBus) dispatch(event Event) tea.Cmd {
    if listener, ok := fb.listeners[event.FactType]; ok {
        return listener.OnFact(event)
    }
    return nil
}
```

---

## Three Integration Layers

| Layer | Transport | Scope | Emitter | Listener |
|-------|-----------|-------|---------|----------|
| **Internal** | `pg` | Same BEAM VM | `{event}_to_pg.erl` | `on_{event}_from_pg_*.erl` |
| **Local** | `tui` (SSE/socket) | Same machine, cross-process | `{event}_to_tui.erl` | `on_{event}_*.go` |
| **External** | `mesh` | WAN, cross-daemon | `{event}_to_mesh.erl` | `on_{event}_from_mesh_*.erl` |

See [INTEGRATION_TRANSPORTS.md](../philosophy/INTEGRATION_TRANSPORTS.md) for the complete transport architecture.

---

## Response Codes

Commands should return **202 Accepted**, not 200 OK:

| Code | Meaning | Use |
|------|---------|-----|
| **200 OK** | "Here's your data" | Queries (GET) |
| **202 Accepted** | "Hope received, processing" | Commands (POST) |
| **400 Bad Request** | "Hope malformed" | Validation errors |
| **409 Conflict** | "Hope contradicts current state" | Business rule violations |

The distinction matters: 200 implies completion, 202 implies the work is still happening.

---

## Implementation Order

1. **Daemon:** Create `tui_bridge.erl` — shared SSE bridge process
2. **Daemon:** Create `{event}_to_tui.erl` emitters for facts the TUI needs
3. **TUI:** Create `FactBus` with SSE subscription (reuse existing StreamParser)
4. **TUI:** Create per-fact listener files (`on_{event}_*.go`)
5. **TUI:** Register listeners with FactBus on startup
6. **TUI:** Move file I/O from command handlers to fact listeners
7. **Daemon:** Return 202 for commands, 200 for queries

---

## Relationship to Other Patterns

- **HOPE/FACT vocabulary** — from Macula mesh protocol, applied to TUI ↔ daemon
- **Per-fact emitters** — same pattern as `_to_pg` and `_to_mesh`, extended with `_to_tui`
- **Projections** — TUI file writes ARE projections, just like SQLite projections in query services
- **Listener placement** — each listener is a vertical unit, not centralized in a `listeners/` directory
- **Transport hiding** — listeners don't know about SSE, just like pg listeners don't know about Erlang distribution

---

*Recorded 2026-02-09. Origin: TUI vision command architecture review.*
*Updated 2026-02-09: Refined from generic event stream to per-fact listener pattern.*
