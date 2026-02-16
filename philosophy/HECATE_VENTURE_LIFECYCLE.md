# The Venture Lifecycle — Process-Centric Architecture

_How Hecate models software development as a set of first-class processes._

**Date:** 2026-02-10 (updated 2026-02-12)
**Status:** Active — supersedes parent-child aggregate pattern
**Origin:** DnA conversation on process-centric vs data-centric architecture

---

## The Insight

Traditional software tools model development as **data management**: you create projects, update records, delete tasks. The verbs are CRUD, the nouns are passive containers.

Hecate models development as a **set of processes**: each phase of building software is its own first-class citizen with its own lifecycle, its own state, and its own dossier. The verbs are business actions, the nouns are active processes.

**The test:** imagine a human sitting at a desk. What lands on their desk? What do they do with it? What do they pass to the next desk?

If the answer is "they manage a database record" — the model is wrong.
If the answer is "they investigate, decide, produce, and hand off" — the model is right.

---

## Terminology

### Hierarchy

```
Venture (1)
  └── Division (N)        — one per bounded context
       └── Department (3)  — CMD, PRJ, QRY
            └── Desk (N)   — individual capability
```

### Domain Concepts

| Term | What It Is | Old Term |
|------|-----------|----------|
| **Venture** | The overall business endeavor — a conglomerate of divisions | Torch |
| **Division** | A specialist firm within the venture, responsible for one bounded context | Cartwheel / Company |
| **Department** | CMD, PRJ, or QRY within a division | Department |
| **Desk** | A single capability within a department (where work gets done) | Spoke |
| **Dossier** | The aggregate — the folder of event slips passing through desks | Dossier |

### Divisions Are Virtual

A division is a **virtual umbrella** — a logical grouping of apps that share a business context. In practice, it maps to either:

1. **Multiple apps within a shared umbrella** (e.g. hecate-daemon)
2. **A separate repo** within the organization

Each division produces N apps following the department pattern:

| Department | Naming | Nature |
|------------|--------|--------|
| CMD | The **process name** itself | Process-centric (verbs) |
| QRY + PRJ | `query_{read_model}` | Data-centric (nouns) |

---

## Three Lifecycle Types

Hecate manages three fundamentally different lifecycle types. Each has its own guide process and its own rhythm.

### 1. Venture Lifecycle (`guide_venture_lifecycle`)

**Scope:** Per venture. **Duration:** Short inception, long-lived discovery.

The venture has two processes:
- `setup_venture` — short-lived, fire-and-done. Birth of the endeavor.
- `discover_divisions` — long-lived, with `open/shelve/resume/conclude` lifecycle. Identifies bounded contexts.

The venture orchestrates divisions. Once divisions are discovered, each follows its own ALC independently.

### 2. Division ALC (`guide_division_alc`)

**Scope:** Per division. **Duration:** Long-lived, cyclical.

The Application Lifecycle — 8 processes that a division cycles through:

| # | Process | Purpose |
|---|---------|---------|
| 1 | `design` | Event storming, aggregate design, architecture |
| 2 | `planning` | Desk inventory, priorities, sequencing |
| 3 | `crafting` | Code generation and implementation |
| 4 | `refactoring` | Structural improvements (can enter anywhere) |
| 5 | `debugging` | Verify quality, acceptance criteria, fix defects |
| 6 | `deployment` | CI/CD, staged rollouts, ship it |
| 7 | `monitoring` | Observe production health, track SLAs |
| 8 | `rescue` | Diagnose incidents, intervene, escalate |

Each process is a first-class aggregate with its own event stream, its own dossier, and its own desks. Long-lived processes use the `open/shelve/resume/conclude` lifecycle protocol.

See **`HECATE_ALC.md`** for detailed event flows, commands, and per-process guides.

### 3. Node Continuous (`guide_node_lifecycle`)

**Scope:** Per node. **Duration:** Indefinite, always-on.

The node lifecycle has no phases and no sub-processes. A node registers, operates forever, and may unpair/re-pair. All desks are independent operations on a living entity:

- `register_identity` — join the mesh
- `configure_node` — set preferences
- `serve_llm` — provide LLM capabilities
- `manage_capabilities` — announce what this node can do
- ...

There is no lifecycle protocol — the node is simply alive and responding to commands.

---

## Lifecycle Protocol

Long-lived processes (venture discovery + all 8 ALC processes) implement:

```
Command:  open_{process}_v1       →  Event: {process}_opened_v1
Command:  shelve_{process}_v1     →  Event: {process}_shelved_v1
Command:  resume_{process}_v1     →  Event: {process}_resumed_v1
Command:  conclude_{process}_v1   →  Event: {process}_concluded_v1
```

**State transitions:**

```
             open
  pending ──────────► active
                        │ ▲
                 shelve │ │ resume
                        ▼ │
                      shelved
                        │
               conclude │
                        ▼
                    concluded
```

**Rules:**
- Domain-specific commands only work when state is `active`
- `shelve` records a reason (blocked, waiting, break)
- `resume` clears the shelve
- `conclude` is the hand-off — facts flow to the next process

---

## Fact Flow Diagram

```
                        guide_venture
                        (orchestrator)
                             ▲
          ┌──────────────────┼──────────────────────┐
          │                  │                       │
   setup_venture    discover_divisions               │
          │                  │                       │
          └────fact──────────┤                       │
                             │                       │
                    ┌────────▼─────────┐             │
                    │ design           │─────fact─────┤
                    └────────┬─────────┘             │
                    ┌────────▼─────────┐             │
                    │ planning         │─────fact─────┤
                    └──┬───────────┬───┘             │
                  ┌────▼──────┐   │                  │
                  │ crafting  │   │                  │
                  └────┬──────┘   │                  │
                  ┌────▼──────┐   │                  │
                  │ debugging │───┤                  │
                  └────┬──────┘   │                  │
                  ┌────▼──────┐   │                  │
                  │ deployment│───┤                  │
                  └────┬──────┘   │                  │
                  ┌────▼──────────▼───┐              │
                  │ monitoring        │──────────────┤
                  └────────┬──────────┘              │
                  ┌────────▼──────────┐              │
                  │ rescue            │──────────────┘
                  └────────┬──────────┘
                           │ escalate
                           ▼
                    design (cycle restarts)
```

Refactoring can enter anywhere in the cycle — it is triggered by any process discovering structural issues. Not shown in main flow.

**Key insight:** the lifecycle is a cycle, not a line:

```
setup → discover → design → planning → crafting → debugging → deployment → monitoring
                      ▲                                                        │
                      └──── rescue ◄───────────────────────────────────────────┘
```

---

## The Guided Conversation Method

### The Protocol

1. **Frame a decision** — Ask a clear, bounded question with no ambiguity
2. **Present options** — Show tradeoffs as a table, not opinions. Include pros AND cons.
3. **User decides** — They own the choice. Never decide for them.
4. **Record the decision** — It becomes a constraint on all future decisions.
5. **Build forward** — Each decision narrows the next decision's option space.
6. **Produce an artifact** — The conversation output is structured data, not prose.

### Phase-Specific Conversations

| Phase | Guided Conversation Produces |
|-------|------------------------------|
| `setup_venture` | Venture name + brief |
| `discover_divisions` | Division list with names, descriptions, boundary rationale |
| `design` | Aggregates, events, stream patterns, status flags |
| `planning` | Desk inventory, types, priorities, sprint sequence |
| `crafting` | Minimal — mechanical, template-driven |
| `refactoring` | Structural analysis, improvement targets, migration plan |
| `debugging` | Test strategy, acceptance criteria, defect diagnosis |
| `deployment` | Release manifest, version, rollout strategy |
| `monitoring` | Health check definitions, SLA thresholds |
| `rescue` | Diagnosis, root cause, fix plan, escalation decision |

### The Decision Cascade

Decisions made in earlier phases constrain later phases:

```
setup: name = "my-saas-app"
  └─ constrains discovery scope

discover: divisions = [auth, billing, notify]
  └─ constrains design: 3 divisions to design

design(auth): aggregates = [user, session, credential]
  └─ constrains planning: desks must cover these aggregates

planning(auth): desks = [register_user, authenticate_user, ...]
  └─ constrains crafting: exactly these desks, in this order
```

Each phase's output is the next phase's input. The conversation at each phase only needs to cover that phase's decisions — everything else is already settled.

---

## What This Replaces

| Old Concept | Replaced By | Why |
|------------|-------------|-----|
| `manage_torches` | `setup_venture` + `guide_venture` | Process, not data management |
| `manage_cartwheels` | Division ALC (8 processes) | Split by phase |
| Parent-child aggregates | Orchestrator + fact flow | No hierarchy, just coordination |
| `torch_aggregate` | `setup` aggregate | Scoped to inception only |
| `cartwheel_aggregate` | Per-phase aggregates | Each phase owns its state |
| `identify_cartwheel` | `discover_division_v1` | Discovery is a process |
| 10-process model | 2 venture + 8 ALC + node continuous | Cleaner separation of concerns |
| start/pause/resume/complete | open/shelve/resume/conclude | More expressive verbs |
| spoke | desk | Work lands on a desk |

---

## Fact Transport

Inter-process communication uses **facts on the mesh** (external) or **pg groups** (internal to same BEAM VM). Processes never call each other directly.

---

*Process-centric architecture. Each phase is a first-class citizen. The lifecycle is a cycle, not a line.*
