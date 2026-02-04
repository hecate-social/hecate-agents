# HECATE_ALC — The Hecate Application Lifecycle

*A four-phase approach to building software with AI assistance.*

---

## Overview

HECATE_ALC defines how Hecate agents approach software development. Four phases, each with clear purpose, activities, and outputs.

```
┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐
│   AnD   │ →  │   AnP   │ →  │   InT   │ →  │   DoO   │
│Analysis │    │  Arch   │    │  Impl   │    │ Deploy  │
│Discovery│    │Planning │    │ Testing │    │  Ops    │
└─────────┘    └─────────┘    └─────────┘    └─────────┘
```

**The wheel turns. Each phase feeds the next.**

---

## The Four Phases

| Phase | Name | Purpose | Detailed Guide |
|-------|------|---------|----------------|
| **AnD** | Analysis & Discovery | Understand the problem | [HECATE_AnD.md](HECATE_AnD.md) |
| **AnP** | Architecture & Planning | Design the solution | [HECATE_AnP.md](HECATE_AnP.md) |
| **InT** | Implementation & Testing | Build it right | [HECATE_InT.md](HECATE_InT.md) |
| **DoO** | Deployment & Operations | Ship and run it | [HECATE_DoO.md](HECATE_DoO.md) |

---

## Phase Summary

### AnD — Analysis & Discovery

*"What problem am I solving?"*

- Understand requirements
- Explore the domain
- Identify constraints and risks
- Research prior art

**Output:** Problem statement, requirements, domain glossary, constraints

---

### AnP — Architecture & Planning

*"What solution will I build?"*

- Model the domain (dossiers, events, commands)
- Design the architecture (spokes, departments)
- Plan the implementation order
- Define interfaces and contracts

**Output:** Domain model, architecture docs, PLAN_*.md, task breakdown

---

### InT — Implementation & Testing

*"Does it work?"*

- Scaffold the codebase
- Set up CI/CD and GitOps
- Implement spokes following templates
- Test and verify continuously

**Output:** Working code, tests, verified builds

**Key Doctrine:** [Walking Skeleton](HECATE_WALKING_SKELETON.md) — Fully operational system from day 1

---

### DoO — Deployment & Operations

*"Is it running well?"*

- Deploy to production
- Monitor and observe
- Respond to incidents
- Collect feedback for next cycle

**Output:** Deployed release, monitoring, feedback log

---

## The Cycle

HECATE_ALC is a cycle, not a waterfall:

```
    ┌──────────────────────────────────────┐
    │                                      │
    ▼                                      │
  AnD → AnP → InT → DoO ──── feedback ────┘
```

After DoO, learnings feed back into the next AnD phase.

**Small cycles, fast feedback.**

---

## Phase Transitions

### AnD → AnP

- [ ] Problem is understood
- [ ] Requirements documented
- [ ] Domain concepts identified
- [ ] Constraints and risks known

### AnP → InT

- [ ] Dossiers and spokes defined
- [ ] Architecture documented
- [ ] Implementation plan ready
- [ ] First iteration scoped

### InT → DoO

- [ ] Code implemented and tested
- [ ] CI/CD pipeline working
- [ ] All verifications passing
- [ ] Release prepared

### DoO → AnD (next cycle)

- [ ] Production stable
- [ ] Feedback collected
- [ ] Metrics analyzed
- [ ] Next priorities identified

---

## Related Doctrines

| Doctrine | Applies To | Description |
|----------|------------|-------------|
| [Walking Skeleton](HECATE_WALKING_SKELETON.md) | InT | Fully operational system from day 1 |
| [Dossier Principle](DDD.md) | AnP | Process-centric domain modeling |
| [Vertical Slicing](VERTICAL_SLICING.md) | AnP, InT | Features live together |
| [Screaming Architecture](SCREAMING_ARCHITECTURE.md) | AnP, InT | Names reveal intent |

---

## For Agents

When working on a project:

1. **Know which phase you're in** — Don't implement during AnD, don't design during InT
2. **Complete the phase** — Finish phase outputs before moving on
3. **Respect the gates** — Check transition criteria
4. **Follow the doctrines** — Walking Skeleton, Vertical Slicing, etc.
5. **Cycle fast** — Small iterations beat big bang

---

*The wheel turns. Each phase feeds the next. The goddess guides the cycle.* 🔥🗝️🔥
