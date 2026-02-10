# The Venture Lifecycle — Process-Centric Architecture

_How Hecate models software development as a set of first-class processes._

**Date:** 2026-02-10
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
  └── Division (N)        — one per business process
       └── Department (3)  — CMD, PRJ, QRY
            └── Desk (N)   — individual capability
```

**Note:** "Division" is a mental model, not an organizational claim. Each division is a cohesive piece of software — a bounded context with clear boundaries. In technical terms, a division maps to an **umbrella app** containing CMD, QRY, and PRJ services. The company metaphor (divisions, departments, desks) helps humans reason about software structure, but the artifact is always code.

### Domain Concepts

| Term | What It Is | Old Term | Why Changed |
|------|-----------|----------|-------------|
| **Venture** | The overall business endeavor — a conglomerate of divisions | Torch | Self-explanatory, no mythology tax |
| **Division** | A specialist firm within the venture, responsible for one business process | Cartwheel / Company | Maps to the Company Model (CMD/PRJ/QRY departments). "Division" avoids self-referential confusion. |
| **Department** | CMD, PRJ, or QRY within a division | Department | Unchanged |
| **Desk** | A single capability within a department (where work gets done) | Spoke | Work lands on a desk. Supports parallel processing (multiple desks on one dossier). |
| **Dossier** | The aggregate — the folder of event slips passing through desks | Dossier | Unchanged (from DDD.md) |

### Process Names

| # | Process | Phase | Scope | Purpose |
|---|---------|-------|-------|---------|
| 1 | `setup_venture` | Inception | Per venture | Birth of the endeavor |
| 2 | `discover_divisions` | Discovery | Per venture | Identify bounded contexts |
| 3 | `design_division` | Architecture | Per division | Event storming, aggregate design |
| 4 | `plan_division` | Planning | Per division | Desk inventory, priorities, sequencing |
| 5 | `generate_division` | Generation | Per division | Skeleton + desk code from templates |
| 6 | `test_division` | Testing | Per division | Verify quality, acceptance criteria |
| 7 | `deploy_division` | Deployment | Per division | CI/CD, staged rollouts, ship it |
| 8 | `monitor_division` | Monitoring | Per division | Observe production health, track SLAs |
| 9 | `rescue_division` | Rescue | Per division | Diagnose, intervene, unblock |
| 10 | `guide_venture` | Orchestration | Per venture | Track progress, coordinate handoffs |

### Why These Verbs

| Verb | Why This Word |
|------|--------------|
| **setup** | Clear, universal. Establish the venture. |
| **discover** | You're finding what's already there in the business domain. |
| **design** | Clear, universal, no explanation needed. |
| **plan** | The work breakdown. What to build, in what order. |
| **generate** | Mechanical. Templates in, code out. Honest. |
| **test** | Verify. Universal. |
| **deploy** | Ship it. CI/CD, rollouts, canaries. |
| **monitor** | Watch it. Universal. |
| **rescue** | Something's wrong, intervene. Urgent, active. |
| **guide** | Hecate is the goddess of crossroads. She guides travelers. |

---

## Process Architecture

### Each Process Is a First-Class Aggregate

Every process has:
- Its own **aggregate** with its own state
- Its own **event stream** (independent lifecycle)
- Its own **dossier** (only the information this desk needs)
- Its own **app directory** in the daemon (desks live within it)

No process contains another process's details. The design dossier doesn't know about code generation. The plan dossier doesn't know about deployment infrastructure. Each desk has exactly the information it needs.

### Aggregate and Stream Mapping

| Process | Aggregate | Stream Pattern | Cardinality |
|---------|-----------|---------------|-------------|
| `setup_venture` | setup | `setup-{venture_id}` | 1 per venture |
| `discover_divisions` | discovery | `discovery-{venture_id}` | 1 per venture |
| `design_division` | design | `design-{division_id}` | N per venture |
| `plan_division` | plan | `plan-{division_id}` | N per venture |
| `generate_division` | generation | `generation-{division_id}` | N per venture |
| `test_division` | testing | `testing-{division_id}` | N per venture |
| `deploy_division` | deployment | `deployment-{division_id}` | N per venture |
| `monitor_division` | monitoring | `monitoring-{division_id}` | N per venture |
| `rescue_division` | rescue | `rescue-{division_id}` | N per venture |
| `guide_venture` | venture | `venture-{venture_id}` | 1 per venture |

---

## Process Lifecycle Protocol

### Short-Lived vs Long-Lived Processes

Not every process needs lifecycle management. A short-lived task is atomic — one command, one event, done. A long-lived process involves ongoing work at a desk: the human sits down, works over time, might get blocked, and eventually finishes.

| Duration | Processes | Lifecycle Protocol |
|----------|-----------|-------------------|
| **Short** | `setup_venture` | No — fire and done |
| **Long** | `discover_divisions`, `design_division`, `plan_division`, `generate_division`, `test_division`, `deploy_division`, `monitor_division`, `rescue_division` | Yes — start/pause/resume/complete |
| **Passive** | `guide_venture` | No — orchestrator, consumes facts |

### The Protocol

Every long-lived process aggregate implements these lifecycle commands:

```
Command:  start_{process}_v1       →  Event: {process}_started_v1
Command:  pause_{process}_v1       →  Event: {process}_paused_v1
Command:  resume_{process}_v1      →  Event: {process}_resumed_v1
Command:  complete_{process}_v1    →  Event: {process}_completed_v1    (already exists)
```

**State transitions:**

```
            start
 pending ──────────► active
                       │ ▲
                 pause │ │ resume
                       ▼ │
                     paused
                       │
              complete │
                       ▼
                   completed
```

**Rules:**
- Domain-specific commands (discover, design, etc.) only work when state is `active`
- `pause` records a reason (blocked, waiting, break)
- `resume` clears the pause
- `complete` is the hand-off — facts flow to the next process

---

## Event Flow

### 1. setup_venture

The birth. User has an idea. **Short-lived** — no lifecycle protocol.

```
Command:  setup_venture_v1
          { venture_id, name, brief, setup_by }

Event:    venture_setup_v1
          { venture_id, name, brief, setup_by, setup_at }

Stream:   setup-{venture_id}

Fact  →   guide_venture
```

**Guided conversation output:** venture name + brief.

### 2. discover_divisions

One discovery per venture. Investigates the domain, identifies N divisions. **Long-lived** — uses lifecycle protocol.

```
Lifecycle: start_discovery_v1 / pause_discovery_v1 / resume_discovery_v1

Command:  discover_division_v1              (repeatable per division found)
          { discovery_id, venture_id, division_name, description, rationale }

Event:    division_discovered_v1
          { discovery_id, venture_id, division_id, division_name,
            description, rationale, discovered_at }

Command:  complete_discovery_v1            (signals: done discovering)
          { discovery_id, venture_id }

Event:    discovery_completed_v1
          { discovery_id, venture_id, division_count, completed_at }

Stream:   discovery-{venture_id}

Facts →   guide_venture
```

**Guided conversation output:** list of divisions with names, descriptions, boundary rationale.

### 3. design_division

Per division. Architecture and event storming. **Long-lived** — uses lifecycle protocol.

```
Lifecycle: start_design_v1 / pause_design_v1 / resume_design_v1

Command:  design_aggregate_v1
          { design_id, division_id, aggregate_name, stream_pattern,
            status_flags, description }

Command:  design_event_v1
          { design_id, division_id, event_name, aggregate_name,
            payload_fields, description }

Command:  complete_design_v1
          { design_id, division_id }

Events:   aggregate_designed_v1
          event_designed_v1
          design_completed_v1

Stream:   design-{division_id}

Facts →   plan_division, guide_venture
```

**Guided conversation output:** aggregates, events, stream patterns, status flags.

### 4. plan_division

Per division. Work breakdown and sequencing. **Long-lived** — uses lifecycle protocol.

```
Lifecycle: start_plan_v1 / pause_plan_v1 / resume_plan_v1

Command:  inventory_desk_v1
          { plan_id, division_id, desk_name, desk_type, priority, description }

Command:  sequence_desks_v1
          { plan_id, division_id, sprint_number, desk_ids }

Command:  complete_plan_v1
          { plan_id, division_id }

Events:   desk_inventoried_v1
          desks_sequenced_v1
          plan_completed_v1

Stream:   plan-{division_id}

Facts →   generate_division, deploy_division, guide_venture

Feedback (facts received):
  ← desk_generated_v1          (from generate_division)
  ← testing_completed_v1       (from test_division)
  ← release_deployed_v1        (from deploy_division)
  ← incident_raised_v1         (from monitor_division)
  ← rescue_completed_v1        (from rescue_division)

Acknowledgement events:
          generation_acknowledged_v1
          testing_acknowledged_v1
          deployment_acknowledged_v1
          incident_acknowledged_v1
          rescue_acknowledged_v1
```

The plan is a **living document**. It knows what's been designed, generated, tested, deployed, and rescued.

**Guided conversation output:** desk inventory with types and priorities, sprint sequencing.

### 5. generate_division

Per division. Template-based code generation. **Long-lived** — uses lifecycle protocol.

```
Lifecycle: start_generation_v1 / pause_generation_v1 / resume_generation_v1

Command:  generate_skeleton_v1
          { generation_id, division_id, workspace_path, language }

Command:  generate_desk_v1
          { generation_id, division_id, desk_id, template_used }

Events:   skeleton_generated_v1
          { generation_id, division_id, files_created, manifest }

          desk_generated_v1
          { generation_id, division_id, desk_id, files_created }

Stream:   generation-{division_id}

Facts →   plan_division (progress feedback), test_division, guide_venture
```

**Minimal conversation needed.** This phase is mechanical — naming conventions + templates = deterministic output.

### 6. test_division

Per division. Verification and quality gates. **Long-lived** — uses lifecycle protocol.

```
Lifecycle: start_testing_v1 / pause_testing_v1 / resume_testing_v1

Command:  run_test_suite_v1
          { testing_id, division_id, suite_type, desk_id }

Command:  record_test_result_v1
          { testing_id, division_id, suite_type, passed, failures, coverage }

Command:  complete_testing_v1
          { testing_id, division_id }

Events:   test_suite_run_v1
          test_result_recorded_v1
          testing_completed_v1

Stream:   testing-{division_id}

Facts →   plan_division (test feedback), deploy_division (quality gate), guide_venture
```

**Guided conversation output:** test strategy, acceptance criteria, coverage requirements.

### 7. deploy_division

Per division. CI/CD, staged rollouts, canary checks. **Long-lived** — uses lifecycle protocol.

```
Lifecycle: start_deployment_v1 / pause_deployment_v1 / resume_deployment_v1

Command:  deploy_release_v1
          { deployment_id, division_id, version, artifacts }

Command:  stage_rollout_v1
          { deployment_id, division_id, stage, percentage }

Command:  complete_deployment_v1
          { deployment_id, division_id }

Events:   release_deployed_v1
          { deployment_id, division_id, version, artifacts, deployed_at }

          rollout_staged_v1
          { deployment_id, division_id, stage, percentage }

          deployment_completed_v1
          { deployment_id, division_id }

Stream:   deployment-{division_id}

Facts →   plan_division (deployment feedback), monitor_division, guide_venture
```

### 8. monitor_division

Per division. Continuous observation after deployment. **Long-lived** — uses lifecycle protocol.

```
Lifecycle: start_monitoring_v1 / pause_monitoring_v1 / resume_monitoring_v1

Command:  register_health_check_v1
          { monitoring_id, division_id, check_type, endpoint, interval }

Command:  record_health_status_v1
          { monitoring_id, division_id, status, metrics, checked_at }

Command:  raise_incident_v1
          { monitoring_id, division_id, severity, description }

Events:   health_check_registered_v1
          health_status_recorded_v1
          incident_raised_v1

Stream:   monitoring-{division_id}

Facts →   rescue_division (when incident raised)
        → plan_division (incident awareness)
        → guide_venture (health overview)
```

### 9. rescue_division

Per division. Triggered by monitoring incidents. **Long-lived** — uses lifecycle protocol.

```
Lifecycle: start_rescue_v1 / pause_rescue_v1 / resume_rescue_v1

Command:  diagnose_incident_v1
          { rescue_id, division_id, incident_id, diagnosis, root_cause }

Command:  apply_fix_v1
          { rescue_id, division_id, fix_description, affected_desks }

Command:  complete_rescue_v1
          { rescue_id, division_id, resolution, lessons_learned }

Command:  escalate_to_redesign_v1
          { rescue_id, division_id, reason, affected_aggregates }

Events:   incident_diagnosed_v1
          fix_applied_v1
          rescue_completed_v1
          redesign_escalated_v1

Stream:   rescue-{division_id}

Facts →   plan_division (rescue feedback)
        → design_division (when escalated to redesign)
        → monitor_division (resolution status)
        → guide_venture (rescue progress)
```

**The feedback loop:** `rescue_division` can escalate back to `design_division`, making the lifecycle circular, not linear.

### 10. guide_venture (Orchestrator)

Consumes facts from ALL processes. Tracks the big picture. **Passive** — no lifecycle protocol (observes, doesn't work).

```
Stream:   venture-{venture_id}

Consumes facts from:
  ← venture_setup             (setup_venture)
  ← division_discovered       (discover_divisions)
  ← discovery_started         (discover_divisions)
  ← discovery_completed       (discover_divisions)
  ← design_started            (design_division)
  ← design_paused             (design_division)
  ← design_completed          (design_division)
  ← plan_started              (plan_division)
  ← plan_completed            (plan_division)
  ← generation_started        (generate_division)
  ← desk_generated            (generate_division)
  ← generation_completed      (generate_division)
  ← testing_started           (test_division)
  ← testing_completed         (test_division)
  ← deployment_started        (deploy_division)
  ← release_deployed          (deploy_division)
  ← deployment_completed      (deploy_division)
  ← monitoring_started        (monitor_division)
  ← incident_raised           (monitor_division)
  ← rescue_started            (rescue_division)
  ← rescue_completed          (rescue_division)
  ← redesign_escalated        (rescue_division)

Venture state:
  #{
    venture_id  => <<"v-123">>,
    name        => <<"my-saas-app">>,
    divisions   => #{
      <<"auth">>    => #{ process => generate_division, state => active, health => ok },
      <<"billing">> => #{ process => design_division, state => paused, health => unknown },
      <<"notify">>  => #{ process => discover_divisions, state => completed, health => unknown }
    }
  }
```

---

## Fact Flow Diagram

```
                        guide_venture
                        (orchestrator)
                             ▲
          ┌──────────────────┼──────────────────────┐
          │                  │                       │
          │    facts from    │    facts from         │    facts from
          │    all divisions │    all divisions      │    all divisions
          │                  │                       │
   setup_venture    discover_divisions               │
          │                  │                       │
          └────fact──────────┤                       │
                             │                       │
                    ┌────────▼─────────┐             │
                    │ design_division  │─────fact─────┤
                    │ (per division)   │             │
                    └────────┬─────────┘             │
                             │ fact                   │
                    ┌────────▼─────────┐             │
                    │  plan_division   │─────fact─────┤
                    │ (per division)   │◄─────────┐  │
                    └──┬──────────┬────┘          │  │
                  fact │          │ fact           │  │
                  ┌────▼──────┐   │               │  │
                  │ generate  │   │               │  │
                  │ _division │   │               │  │
                  └────┬──────┘   │               │  │
                  fact │          │               │  │
                  ┌────▼──────┐   │               │  │
                  │ test      │   │               │  │
                  │ _division │───┤               │  │
                  └────┬──────┘   │               │  │
            quality    │          │               │  │
              gate     │          │               │  │
                  ┌────▼──────┐   │               │  │
                  │ deploy    │   │               │  │
                  │ _division │───┤               │  │
                  └────┬──────┘   │               │  │
                       │ fact     │               │  │
                       └──►plan◄──┘               │  │
                                                  │  │
                    ┌──────────────────┐           │  │
                    │ monitor_division │────fact────┤  │
                    │ (per division)   │           │  │
                    └────────┬─────────┘           │  │
                             │ alert                │  │
                    ┌────────▼─────────┐           │  │
                    │ rescue_division  │────fact────┘  │
                    │ (per division)   │───────────────┘
                    └────────┬─────────┘
                             │
                             │ escalate (when design flaw)
                             ▼
                      design_division (cycle restarts)
```

**Key insight:** the lifecycle is NOT linear. It's a cycle:
```
setup → discover → design → plan → generate → test → deploy → monitor
                      ▲                                           │
                      └──── rescue ◄──────────────────────────────┘
```

---

## The Guided Conversation Method

### What We Discovered

The process of defining this architecture WAS the method. Each phase's "guided conversation" follows the same protocol.

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
| `design_division` | Aggregates, events, stream patterns, status flags |
| `plan_division` | Desk inventory, types, priorities, sprint sequence |
| `generate_division` | Minimal — mechanical, template-driven |
| `test_division` | Test strategy, acceptance criteria, coverage requirements |
| `deploy_division` | Release manifest, version, rollout strategy |
| `monitor_division` | Health check definitions, SLA thresholds |
| `rescue_division` | Diagnosis, root cause, fix plan, escalation decision |

### The Decision Cascade

Decisions made in earlier phases constrain later phases. This is by design — it compresses cognitive load:

```
setup: name = "my-saas-app"
  └─ constrains discovery scope

discover: divisions = [auth, billing, notify]
  └─ constrains design: 3 divisions to design

design(auth): aggregates = [user, session, credential]
  └─ constrains planning: desks must cover these aggregates

plan(auth): desks = [register_user, authenticate_user, ...]
  └─ constrains generation: exactly these desks, in this order

generate(auth): skeleton + desk files
  └─ constrains testing: these files, these acceptance criteria

test(auth): all suites pass, coverage met
  └─ constrains deployment: quality gate passed

deploy(auth): v0.1.0 staged rollout
  └─ constrains monitoring: these endpoints, these SLAs
```

Each phase's output is the next phase's input. The conversation at each phase only needs to cover that phase's decisions — everything else is already settled.

---

## What This Replaces

| Old Concept | Replaced By | Why |
|------------|-------------|-----|
| `manage_torches` | `setup_venture` + `guide_venture` | Process, not data management |
| `manage_cartwheels` | `design_division` + `plan_division` + `generate_division` | Split by phase |
| Parent-child aggregates | Orchestrator + fact flow | No hierarchy, just coordination |
| `torch_aggregate` | `setup` aggregate | Scoped to inception only |
| `cartwheel_aggregate` | Per-phase aggregates (design, plan, generation, etc.) | Each phase owns its state |
| `identify_cartwheel` command | `discover_division_v1` command | Discovery is a process, not a parent identifying a child |
| `PARENT_CHILD_AGGREGATES.md` | This document | Process-centric replaces data-centric |

---

## Mapping to ALC Phases

| ALC Phase | Processes |
|-----------|----------|
| **DnA** (Discovery & Analysis) | `setup_venture`, `discover_divisions` |
| **AnP** (Architecture & Planning) | `design_division`, `plan_division` |
| **TnI** (Testing & Implementation) | `generate_division`, `test_division` |
| **DnO** (Deployment & Operations) | `deploy_division`, `monitor_division`, `rescue_division` |
| **Cross-cutting** | `guide_venture` (orchestrator) |

---

## User-Facing Commands

```bash
# Inception
/setup venture my-saas-app "A multi-tenant SaaS platform"

# Discovery
/discover divisions
# → guided conversation identifies bounded contexts

# Architecture (per division)
/design division auth
# → guided conversation: event storming, aggregates

# Planning (per division)
/plan division auth
# → guided conversation: desk inventory, priorities

# Generation (per division)
/generate division auth
# → mechanical: templates + naming conventions = code

# Testing (per division)
/test division auth
# → run test suites, verify acceptance criteria

# Deployment (per division)
/deploy division auth v0.1.0
# → CI/CD build, staged rollout

# Monitoring
/monitor division auth
# → sets up health checks, SLA tracking

# Rescue
/rescue division auth
# → diagnose incident, apply fix or escalate
```

---

## Implementation Notes

### Divisions Are Virtual

A division is NOT a single OTP app. It's a **virtual umbrella** — a logical grouping of apps that share a business process. In practice, a division maps to either:

1. **Multiple apps within a shared umbrella** (e.g. hecate-daemon) — the existing pattern
2. **A separate repo** within the organization — for large or independently deployable divisions

Each division produces N apps following the department pattern:

| Department | Naming | Nature | Example (design_division) |
|------------|--------|--------|--------------------------|
| CMD | The **process name** itself | Process-centric (verbs) | `design_division` |
| QRY + PRJ | `query_{read_model}` | Data-centric (nouns) | `query_designs` |

The CMD app name IS the division's process name — no `manage_` prefix. The verb already screams intent.

### Daemon Apps

```
apps/
├── setup_venture/           ← CMD: inception process
├── discover_divisions/      ← CMD: discovery process
├── query_discoveries/       ← QRY+PRJ: discovery read model
├── design_division/         ← CMD: architecture process
├── query_designs/           ← QRY+PRJ: design read model
├── plan_division/           ← CMD: planning process
├── query_plans/             ← QRY+PRJ: plan read model
├── generate_division/       ← CMD: generation process
├── query_generations/       ← QRY+PRJ: generation read model
├── test_division/           ← CMD: testing process
├── query_tests/             ← QRY+PRJ: test read model
├── deploy_division/         ← CMD: deployment process
├── query_deployments/       ← QRY+PRJ: deployment read model
├── monitor_division/        ← CMD: monitoring process
├── query_monitoring/        ← QRY+PRJ: monitoring read model
├── rescue_division/         ← CMD: rescue process
├── query_rescues/           ← QRY+PRJ: rescue read model
└── guide_venture/           ← orchestrator (read-only, consumes facts)
```

### Vertical Slicing

Each app contains all its desks. No shared `services/`, `handlers/`, or `listeners/` directories.

### Fact Transport

Inter-process communication uses **facts on the mesh** (external) or **pg groups** (internal to same BEAM VM). Processes never call each other directly.

---

*Process-centric architecture. Each phase is a first-class citizen. The lifecycle is a cycle, not a line.*
