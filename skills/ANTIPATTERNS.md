# ANTIPATTERNS.md — Demons We've Exorcised

*Mistakes we've made and corrected. Read this. Don't repeat them.*

---

## 🔥 Technical Names Don't Scream

**Date:** 2026-02-04  
**Origin:** hecate-daemon apprentice

### The Antipattern

Slice names that describe **HOW** (technical implementation) instead of **WHAT** (business intent):

| Technical Name (BAD) | What It Actually Does |
|---------------------|----------------------|
| `llm_model_poller` | Detects what models Ollama has |
| `llm_rpc_listener` | Handles incoming LLM requests from mesh |
| `*_listener` | Technical concern, not business intent |
| `*_poller` | Technical concern, not business intent |
| `*_handler` | Technical concern, not business intent |
| `*_worker` | Technical concern, not business intent |

These names are **horizontal thinking in disguise**. They describe the mechanism, not the meaning.

### The Rule

> **Slice names must be VERB PHRASES that scream WHAT the slice does, not HOW it does it.**

| ❌ Technical (HOW) | ✅ Screaming (WHAT) |
|-------------------|---------------------|
| `poll_llm_models/` | `detect_llm_models/` |
| `handle_llm_rpc/` | `listen_for_llm_request/` |
| `capability_listener/` | `discover_remote_capability/` |
| `event_handler/` | `on_capability_announced_update_index/` |

### Why It Matters

A stranger reading your folder structure should understand **what your system does**, not **what frameworks you used**.

```
# BAD — I see plumbing
apps/serve_llm/src/
├── poll_llm_models/
├── handle_llm_rpc/
└── emit_llm_events/

# GOOD — I see capabilities  
apps/serve_llm/src/
├── detect_llm_models/
├── listen_for_llm_request/
└── announce_llm_availability/
```

### The Test

Read your slice name aloud. If it sounds like infrastructure, rename it.

- "This slice *polls models*" → Infrastructure. ❌
- "This slice *detects available LLM models*" → Business. ✅

---

## 🔥 Parallel Domain Infrastructure

**Date:** 2026-02-04  
**Origin:** hecate-daemon apprentice

### The Antipattern

Creating duplicate command/event/emitter infrastructure in a new domain when an existing domain already handles that concept.

**Example:** `serve_llm` created:
- `announce_llm_capability_v1` command
- `llm_capability_announced_v1` event  
- `llm_capability_announced_v1_to_mesh` emitter
- `hecate.llm.announced` mesh topic

But `manage_capabilities` already has:
- `announce_capability_v1` command
- `capability_announced_v1` event
- `capability_announced_v1_to_mesh` emitter
- `hecate.capability.announced` mesh topic

LLM capabilities ARE just capabilities with `type = <<"llm">>`.

### The Rule

> **Don't duplicate domain concepts. Extend existing domains or use Process Managers to integrate.**

### The Solution: Process Managers

When Domain A needs to trigger behavior in Domain B:

```
Domain A (serve_llm)
    ↓ emits internal event (llm_model_detected_v1)

Process Manager (on_llm_model_detected_announce_capability)
    ↓ subscribes to Domain A events
    ↓ dispatches command to Domain B

Domain B (manage_capabilities)
    ↓ handles command normally
    ↓ existing infrastructure does the rest
```

**Loose coupling. Single source of truth. No duplication.**

---

## 🔥 Incomplete Spokes / Flat Workers

**Date:** 2026-02-04  
**Origin:** hecate-daemon architecture review

### The Antipattern

CMD slices that are missing components or have workers directly supervised by the domain supervisor.

**Symptoms:**
```erlang
%% BAD: Domain sup directly supervises workers
manage_capabilities_sup
├── capability_announced_v1_to_mesh   % Worker — WRONG LEVEL
├── remote_capabilities_listener      % Worker — WRONG LEVEL
└── ...
```

**Missing pieces:**
- No spoke supervisor (`*_spoke_sup.erl`)
- No responder (`*_responder_v1.erl`) — can't receive HOPEs from mesh
- Emitters exist but float orphaned at domain level

### The Rule

> **Domain supervisors ONLY start spoke supervisors + shared infra.**
> **Spoke supervisors start all workers for that spoke.**

```erlang
%% GOOD: Domain sup → Spoke sups → Workers
manage_capabilities_sup
├── manage_capabilities_store         % Shared infra (OK at domain level)
├── announce_capability_spoke_sup     % Supervisor
│   ├── announce_capability_responder_v1    % Worker
│   └── capability_announced_v1_to_mesh     % Worker
├── update_capability_spoke_sup       % Supervisor
│   └── ...
└── retract_capability_spoke_sup      % Supervisor
    └── ...
```

### Complete Spoke Checklist

Every CMD spoke MUST have:
- [ ] `*_spoke_sup.erl` — Spoke supervisor
- [ ] `*_v1.erl` — Command record
- [ ] `*_v1.erl` — Event record  
- [ ] `maybe_*.erl` — Handler
- [ ] `*_responder_v1.erl` — HOPE → Command (mesh inbound)
- [ ] `*_to_mesh.erl` — Event → FACT emitter (mesh outbound)

Optional:
- [ ] `on_{event}_maybe_*.erl` — Policy/PM for cross-domain

### Why It Matters

Without responders, your domain can emit but not receive. You have a mouth but no ears.

Without spoke supervisors, your supervision tree is flat and you lose fault isolation per feature.

See `~/work/github.com/CARTWHEEL.md` for the complete canonical structure.

---

## 🔥 Missing or Wrong "Birth" Event

**Date:** 2026-02-08
**Origin:** Torch/Cartwheel architecture discussion

### The Antipattern

Process/domain aggregates that don't have a clear "birth" event, or use wrong verbs:

| ❌ Wrong | Why |
|----------|-----|
| `project_created_v1` | CRUD — "created" says nothing about business intent |
| `cartwheel_started_v1` | Ambiguous — "started" could mean resumed, begun, etc. |
| `order_made_v1` | Weak — doesn't convey the initiation of a process |
| No birth event at all | Aggregate appears from nowhere |

### The Rule

> **Every process/domain aggregate MUST begin with `{aggregate_singular}_initiated_v{N}`**

This event is the "birth" of the aggregate — it marks the moment the process began.

| ✅ Correct | Aggregate |
|-----------|-----------|
| `torch_initiated_v1` | Torch |
| `cartwheel_initiated_v1` | Cartwheel |
| `order_initiated_v1` | Order |
| `claim_initiated_v1` | Insurance Claim |
| `project_initiated_v1` | Project |

### Why "Initiated"?

- **Business verb** — "We initiated a new project" is natural language
- **Process-oriented** — signals the START of a lifecycle, not just creation of data
- **Consistent** — one word for all aggregates, easy to search/grep
- **Not CRUD** — avoids the banned "created/updated/deleted" vocabulary

### The Pattern

```erlang
%% First event in any aggregate stream
#{
    event_type => <<"torch_initiated_v1">>,
    stream_id => <<"torch-abc123">>,
    data => #{
        torch_id => <<"abc123">>,
        name => <<"macula-geo">>,
        brief => <<"Geo-restriction for compliance">>,
        initiated_by => <<"human:rl">>,
        initiated_at => 1707350400000
    }
}
```

### Checklist

When creating a new aggregate:
- [ ] First event is `{aggregate}_initiated_v1`
- [ ] Event includes `initiated_by` (who/what started it)
- [ ] Event includes `initiated_at` (timestamp)
- [ ] Handler validates "not already initiated" before accepting

---

## 🔥 Auto-Creating Child Aggregates on Parent Initiation

**Date:** 2026-02-08
**Origin:** Torch → Cartwheel architecture discussion

### The Antipattern

Assuming that when a parent aggregate is initiated, child aggregates should be automatically created.

**Example (WRONG):**
```
torch_initiated_v1
    → listener subscribes
    → automatically creates cartwheel
```

This assumes:
- Every torch needs exactly one cartwheel
- The relationship is 1:1 and automatic
- No human/agent decision is needed

**Reality:** A torch might need 0, 1, 5, or 10 cartwheels. This is a deliberate decision, not an automatic consequence.

### The Rule

> **Parent aggregates IDENTIFY children. Child aggregates INITIATE themselves.**

The parent owns the "what exists" decision. The child owns its lifecycle.

### The Correct Flow

```
1. torch_initiated_v1           # Torch exists
2. cartwheel_identified_v1      # Torch decides "I need a cartwheel called X"
   → emitted to mesh
3. cartwheel_initiated_v1       # Cartwheel service starts X's lifecycle
```

### Semantic Distinction

| Action | Owner | Meaning |
|--------|-------|---------|
| **Identify** | Parent (torch) | "This parent will have a child called X" |
| **Initiate** | Child (cartwheel) | "Start the lifecycle of X" |

### Why It Matters

- **Flexibility** — Parent can have 0..N children
- **Explicit decisions** — Humans/agents choose what children exist
- **Proper DDD** — Parent aggregate owns its children's identities
- **Separation of concerns** — Identity vs lifecycle are different responsibilities

### The Lesson

> **Skip DnA phase → Make wrong assumptions → Build wrong architecture → Waste time fixing**

This mistake happened because we jumped to implementation without understanding the domain.

---

## 🔥 Listeners as Separate Spokes

**Date:** 2026-02-08
**Origin:** Cartwheel listener architecture discussion

### The Antipattern

Creating a listener as its own spoke when it only serves one other spoke.

**Example (WRONG):**
```
manage_cartwheels/src/
├── subscribe_to_cartwheel_identified/     # Separate spoke
│   ├── subscribe_to_cartwheel_identified.erl
│   └── subscribe_to_cartwheel_identified_sup.erl
│
└── initiate_cartwheel/                    # The spoke it triggers
    ├── initiate_cartwheel_v1.erl
    └── maybe_initiate_cartwheel.erl
```

This is **horizontal thinking in disguise** — grouping by "listeners" vs "commands".

### The Rule

> **If a listener's sole purpose is to trigger spoke X, it lives IN spoke X.**

### The Correct Structure

```
manage_cartwheels/src/
└── initiate_cartwheel/
    ├── initiate_cartwheel_v1.erl
    ├── cartwheel_initiated_v1.erl
    ├── maybe_initiate_cartwheel.erl
    ├── subscribe_to_cartwheel_identified.erl           # Lives here
    └── on_cartwheel_identified_maybe_initiate_cartwheel.erl  # Lives here
```

The spoke owns **everything needed to initiate cartwheels** — including how it gets triggered.

### When Listeners CAN Be Separate

A listener MAY be its own spoke when:
- It triggers **multiple** different spokes based on message content
- It's truly general-purpose infrastructure (rare)
- It serves a query service, not a command service

### The Test

Ask: "Does this listener exist ONLY to trigger spoke X?"
- **Yes** → Put it in spoke X
- **No** → Consider a separate spoke (but think hard)

---

## 🔥 Using Mesh for Internal Integration

**Date:** 2026-02-08
**Origin:** hecate-daemon walking skeleton debugging

### The Antipattern

Using Macula mesh (`*_to_mesh.erl` emitters) for communication between umbrella apps within the same BEAM VM.

**Example (WRONG):**
```
manage_torches (CMD app)
    → torch_initiated_v1_to_mesh.erl
    → Macula mesh (QUIC, DHT, NAT traversal)
    → on_torch_initiated_from_mesh.erl
    → query_torches (PRJ+QRY app)
```

This uses WAN-grade infrastructure for intra-process communication.

### Why It's Wrong

1. **Massive overhead** — QUIC, DHT discovery, NAT traversal for processes in the same VM
2. **Wrong tool** — Mesh is designed for WAN, agent-to-agent communication
3. **Doesn't work in K8s** — Container networking breaks mesh protocols
4. **Adds latency** — Network round-trip for what should be direct message passing

### The Rule

> **Use `pg` (OTP process groups) for internal integration. Reserve `mesh` for external/WAN integration.**

### The Correct Pattern

```
manage_torches (CMD app)
    → torch_initiated_v1_to_pg.erl    # Internal via pg
    → Direct Erlang message passing
    → on_torch_initiated_v1_from_pg_project_to_sqlite_torches.erl
    → query_torches (PRJ+QRY app)
```

### Two Integration Layers

| Layer | Transport | Scope |
|-------|-----------|-------|
| **Internal** | `pg` | Same BEAM VM, intra-daemon |
| **External** | `mesh` | WAN, cross-daemon, agent-to-agent |

### Naming Convention

| Transport | Emitter | Listener |
|-----------|---------|----------|
| pg | `{event}_to_pg.erl` | `on_{event}_from_pg_*.erl` |
| mesh | `{event}_to_mesh.erl` | `on_{event}_from_mesh_*.erl` |

See [INTEGRATION_TRANSPORTS.md](../philosophy/INTEGRATION_TRANSPORTS.md) for full details.

---

## 🔥 Centralized Listener Supervisors

**Date:** 2026-02-08
**Origin:** hecate-daemon architecture refinement

### The Antipattern

Creating a central supervisor for all listeners across domains.

**Example (WRONG):**
```
apps/hecate_listeners/src/
├── hecate_listeners_sup.erl          # Central supervisor
├── torch_initiated_listener.erl
├── cartwheel_identified_listener.erl
└── capability_announced_listener.erl
```

Or within a domain:
```
apps/manage_cartwheels/src/
├── manage_cartwheels_listeners_sup.erl   # Still wrong!
├── listeners/                             # Horizontal directory
│   ├── cartwheel_identified_listener.erl
│   └── ...
```

### The Rule

> **Each listener belongs to the spoke it triggers, supervised by that spoke's supervisor.**

### The Correct Structure

```
apps/manage_cartwheels/src/
├── initiate_cartwheel/
│   ├── initiate_cartwheel_sup.erl                              # Spoke supervisor
│   └── on_cartwheel_identified_v1_from_pg_maybe_initiate_cartwheel.erl
│
└── complete_cartwheel/
    ├── complete_cartwheel_sup.erl                              # Spoke supervisor
    └── on_all_spokes_implemented_v1_from_pg_maybe_complete_cartwheel.erl
```

### Why It Matters

- **Fault isolation** — Listener crash only affects its spoke
- **Discoverability** — To understand spoke X, look only in `X/`
- **No orphans** — Every listener has a clear owner
- **Vertical slicing** — No horizontal grouping by technical concern

See [INTEGRATION_TRANSPORTS.md](../philosophy/INTEGRATION_TRANSPORTS.md) for spoke structures.

---

## 🔥 Direct Creation Endpoints for Child Aggregates

**Date:** 2026-02-09
**Origin:** Cartwheel API architecture review

### The Antipattern

Exposing a direct "create" or "initiate" API endpoint for child aggregates that should only exist through their parent.

**Example (WRONG):**
```
POST /api/cartwheels/initiate
{
  "name": "My Cartwheel",
  "description": "..."
}
```

This bypasses the domain flow:
- Cartwheels should only exist because a torch identified them
- Direct creation allows orphan cartwheels (no parent torch)
- Violates the parent-child aggregate pattern

### The Rule

> **Child aggregates should NOT have direct creation endpoints. They're created through parent identification.**

### The Correct Flow

1. **Parent identifies child**: `POST /api/torches/:torch_id/cartwheels/identify`
2. **Event emitted**: `cartwheel_identified_v1` to pg (internal) + mesh (external)
3. **Listener receives**: `subscribe_to_cartwheel_identified` in manage_cartwheels
4. **Policy decides**: `on_cartwheel_identified_maybe_initiate_cartwheel`
5. **Child initiated**: `cartwheel_initiated_v1` event created

### When to Use Which

| Aggregate Type | Creation Endpoint? | Why |
|----------------|-------------------|-----|
| **Root aggregate** | ✅ Yes | Torch, Order, User — top-level entities |
| **Child aggregate** | ❌ No | Cartwheel, OrderLine — created via parent |

### API Design Pattern

```erlang
%% GOOD: Parent creates children through relationship
POST /api/torches/:torch_id/cartwheels/identify

%% BAD: Direct creation of child
POST /api/cartwheels/initiate    % REMOVE THIS
```

### Testing Implications

When testing the walking skeleton:
- **Respect the domain flow** — Don't bypass event-driven creation with direct APIs
- **Test the full path** — Parent → Event → Listener → Child
- **If it doesn't work** — Fix the event flow, don't add a shortcut API

### The Lesson

> **API endpoints should reflect domain operations, not CRUD convenience.**

If you need to create test data, use the proper flow or seed the event store directly.

---

## 🔥 Wrong Aggregate Callback Argument Order

**Date:** 2026-02-09
**Origin:** hecate-daemon cartwheel auto-initiation bug

### The Antipattern

Writing aggregate `execute/2` and `apply/2` callbacks with wrong argument order.

**Example (WRONG):**
```erlang
%% WRONG - Payload first, State second
execute(#{command_type := <<"my_command">>} = Payload, State) ->
    do_something(Payload, State).

apply_event(#{event_type := <<"my_event_v1">>} = Event, State) ->
    update_state(Event, State).
```

### The Rule

> **evoq behaviour callbacks expect: State first, then Payload/Event.**

evoq_aggregate.erl calls:
```erlang
Module:execute(AggState, Command#evoq_command.payload)
Module:apply(AccState, Event)
```

### The Correct Implementation

```erlang
-module(my_aggregate).
-behaviour(evoq_aggregate).

%% Behaviour callbacks
-export([init/1, execute/2, apply/2]).

%% init/1 returns {ok, State}
init(_AggregateId) ->
    {ok, initial_state()}.

%% execute/2: State first, Payload second
execute(State, #{command_type := <<"my_command">>} = Payload) ->
    do_something(State, Payload).

%% apply/2: State first, Event second
apply(State, #{event_type := <<"my_event_v1">>} = Event) ->
    update_state(State, Event).
```

### Why This Happens

1. It's natural to write `execute(CommandPayload, State)` — "execute this command on this state"
2. Many examples online show the wrong order
3. Without tests, the bug only appears at runtime

### The Symptom

All commands fail with `{error, unknown_command}` because:
- evoq passes `(State, Payload)`
- Aggregate receives State where it expects a map
- Pattern match `#{command_type := ...}` fails on a record
- Falls through to catch-all: `execute(_State, _Payload) -> {error, unknown_command}`

### Prevention: Always Test Aggregates

```erlang
execute_argument_order_test() ->
    State = my_aggregate:initial_state(),
    Payload = #{command_type => <<"my_command">>, id => <<"test">>},

    %% This test catches wrong argument order immediately
    Result = my_aggregate:execute(State, Payload),
    ?assertMatch({ok, [_]}, Result).
```

### The Lesson

> **Use `-behaviour(evoq_aggregate).`** — The compiler will check callbacks exist.
> **Write aggregate tests before push** — A 10-line test would have caught this bug.

---

*Add more demons as we exorcise them.* 🔥🗝️🔥
