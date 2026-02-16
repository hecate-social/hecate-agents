# HECATE ALC -- The Division Application Lifecycle

*Eight processes. One cycle. The wheel turns for every division.*

---

## Overview

The ALC governs how a **division** (bounded context, cohesive piece of software) evolves from idea to production and back again. It replaces the old four-phase model (DnA/AnP/TnI/DnO) with eight independent, long-lived processes -- each with its own aggregate, its own lifecycle, its own voice.

The ALC applies to **divisions specifically**. Ventures have their own lifecycle. Nodes run continuously. The division is where craft happens, and the ALC is the rhythm of that craft.

---

## The Eight Processes

| # | Process | Purpose | Replaces |
|---|---------|---------|----------|
| 1 | **Design** | Discover the domain, model aggregates and events, event storming | DnA |
| 2 | **Planning** | Plan desk capabilities, sequence work, define inboxes/policies/emitters | AnP |
| 3 | **Crafting** | Generate code, scaffold structures, write implementations | TnI (generate) |
| 4 | **Refactoring** | Planned structural improvement of existing code | *New* |
| 5 | **Debugging** | Test, investigate failures, verify correctness | TnI (test) |
| 6 | **Deployment** | CI/CD, staged rollouts, ship to environments | DnO (deploy) |
| 7 | **Monitoring** | Observe health, collect metrics, detect anomalies | DnO (monitor) |
| 8 | **Rescue** | Incident response, hotfixes, escalation back to design | DnO (rescue) |

Each process is a first-class citizen -- its own CMD app, its own dossier, its own desks.

---

## Lifecycle Protocol

Every process is long-lived. Every process speaks with **screaming verbs** -- not generic `start_phase` / `pause_phase`, but verbs that name the process they govern.

```
open_{process}     --> {process}_opened_v1       (begin work)
shelve_{process}   --> {process}_shelved_v1       (blocked, set aside)
resume_{process}   --> {process}_resumed_v1       (pick back up)
conclude_{process} --> {process}_concluded_v1     (hand off to next)
```

### State Machine

```
pending --> active --> paused --> completed
  |          |          |
  |       shelve     resume
  |          |          |
  open       +--paused--+
                         |
                      conclude
                         |
                      completed
```

A process that has never been opened is `pending`. Opening it makes it `active`. Shelving pauses it. Resuming reactivates it. Concluding completes it and signals readiness for the next process.

See **Demon #16** in [ANTIPATTERNS.md](../skills/ANTIPATTERNS.md) for why generic lifecycle verbs are forbidden.

---

## The Cycle

The eight processes form a cycle. Feedback flows backward through rescue, which can escalate all the way to design.

```
  design --> planning --> crafting --> debugging --> deployment --> monitoring
    ^                                                                 |
    |                                                                 v
    +------------------------- rescue <-------------------------------+
```

**Refactoring** is the wildcard. It enters the cycle wherever structural improvement is needed -- after debugging reveals tangled code, after monitoring reveals performance rot, after design reveals a better model. It feeds back into crafting or deployment depending on scope.

```
  monitoring ---> refactoring ---> crafting
  debugging  ---> refactoring ---> crafting
  design     ---> refactoring ---> crafting
```

The cycle is not a waterfall. Processes overlap. A division may have active design *and* active monitoring simultaneously -- the goddess holds many threads.

---

## CMD App Naming

Each process gets its own CMD app, named after what it does:

| Process | CMD App | QRY+PRJ App |
|---------|---------|-------------|
| Design | `design_division` | `query_designs` |
| Planning | `plan_division` | `query_plans` |
| Crafting | `craft_division` | `query_crafts` |
| Refactoring | `refactor_division` | `query_refactors` |
| Debugging | `debug_division` | `query_debugs` |
| Deployment | `deploy_division` | `query_deployments` |
| Monitoring | `monitor_division` | `query_monitors` |
| Rescue | `rescue_division` | `query_rescues` |

No `manage_` prefix. CMD supports a **process**, not data management.

---

## Per-Process Detail Files

Each process has its own deep-dive guide:

| Process | Guide |
|---------|-------|
| Design | [HECATE_ALC_DESIGN.md](HECATE_ALC_DESIGN.md) |
| Planning | [HECATE_ALC_PLANNING.md](HECATE_ALC_PLANNING.md) |
| Crafting | [HECATE_ALC_CRAFTING.md](HECATE_ALC_CRAFTING.md) |
| Refactoring | [HECATE_ALC_REFACTORING.md](HECATE_ALC_REFACTORING.md) |
| Debugging | [HECATE_ALC_DEBUGGING.md](HECATE_ALC_DEBUGGING.md) |
| Deployment | [HECATE_ALC_DEPLOYMENT.md](HECATE_ALC_DEPLOYMENT.md) |
| Monitoring | [HECATE_ALC_MONITORING.md](HECATE_ALC_MONITORING.md) |
| Rescue | [HECATE_ALC_RESCUE.md](HECATE_ALC_RESCUE.md) |

This file is the index. The detail files describe activities, outputs, and transition criteria.

---

## Three Lifecycles

The ALC is one of three lifecycle types in the Hecate ecosystem:

| Lifecycle | Scope | Nature |
|-----------|-------|--------|
| **Venture Lifecycle** | The overall business endeavor | Setup, discovery, orchestration |
| **Division ALC** | A single bounded context | The eight-process cycle described here |
| **Node Lifecycle** | Infrastructure | Continuous operation, no phases |

See [HECATE_VENTURE_LIFECYCLE.md](HECATE_VENTURE_LIFECYCLE.md) for the venture-level view.

---

## Related Doctrines

| Doctrine | Relevance | Description |
|----------|-----------|-------------|
| [Walking Skeleton](HECATE_WALKING_SKELETON.md) | Crafting, Debugging | Fully operational system from day one |
| [Dossier Principle](DDD.md) | Design, Planning | Process-centric domain modeling |
| [Vertical Slicing](VERTICAL_SLICING.md) | Planning, Crafting | Features live together, no horizontal layers |
| [Screaming Architecture](SCREAMING_ARCHITECTURE.md) | Planning, Crafting | Names reveal intent |
| [Division Model](../guides/CARTWHEEL_COMPANY_MODEL.md) | All | CMD/PRJ/QRY department structure |

---

## For Agents

When working on a division:

1. **Know which process is active.** Do not craft during design. Do not design during deployment. Each process has a purpose -- respect it.
2. **Use the lifecycle verbs.** `open_design`, `conclude_planning`, `shelve_debugging` -- not `start`, `stop`, `pause`. The process screams its name.
3. **Follow the cycle.** Design before planning. Plan before crafting. Debug before deploying. The order exists for a reason.
4. **Refactoring is intentional.** It is never accidental cleanup buried in a feature branch. Open the refactoring process explicitly.
5. **Rescue escalates.** When monitoring finds rot, rescue opens. When rescue finds architectural flaws, design reopens. The cycle feeds itself.
6. **Cycle fast.** Small iterations. Conclude early. Open the next process. The goddess favors momentum over perfection.

---

## Terminology

| Term | Meaning | Old Term |
|------|---------|----------|
| **Venture** | The overall business endeavor | Torch |
| **Division** | A bounded context, cohesive software unit | Cartwheel / Company |
| **Department** | CMD, PRJ, or QRY within a division | Department |
| **Desk** | A single capability within a department | Spoke |
| **Dossier** | The aggregate -- folder of event slips | Dossier |

---

*The wheel turns. Eight spokes now, not four. Each process feeds the next. The goddess guides the cycle.*
