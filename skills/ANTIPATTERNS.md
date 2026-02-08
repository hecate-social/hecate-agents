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

*Add more demons as we exorcise them.* 🔥🗝️🔥
