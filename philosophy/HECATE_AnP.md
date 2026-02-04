# HECATE_AnP — Architecture & Planning

*Design the solution before building it.*

**Phase 2 of [HECATE_ALC](HECATE_ALC.md)**

---

## Purpose

Before writing code, design:
- What dossiers exist?
- What spokes process them?
- What's the system structure?
- What's the implementation order?

**AnP transforms understanding into a buildable plan.**

---

## Mindset

```
"I design in dossiers and spokes.
 I think vertically, not horizontally.
 I plan for the Walking Skeleton first.
 I document decisions for future me."
```

---

## Activities

### 1. Domain Modeling

**Apply the Dossier Principle** (see [DDD.md](DDD.md))

For each domain:

| Question | Answer |
|----------|--------|
| What dossiers exist? | Things that accumulate history |
| What desks process them? | Actions/commands that can happen |
| What slips are added? | Events that record what happened |
| What index cards do we need? | Projections for querying |

**Example: Capability Domain**

```
Dossier: capability-{mri}
├── Desks: announce, update, revoke, endorse
├── Slips: announced, updated, revoked, endorsed
└── Index Cards: by_agent, by_tag, by_status
```

---

### 2. Architecture Design

**Structure into departments** (see [CARTWHEEL.md](CARTWHEEL.md))

```
{domain}/
├── CMD Department
│   ├── {command}_spoke/
│   └── ...
├── PRJ Department
│   ├── {event}_to_{table}/
│   └── ...
└── QRY Department
    ├── {query}/
    └── ...
```

**Apply Vertical Slicing** (see [VERTICAL_SLICING.md](VERTICAL_SLICING.md))
- Each spoke is self-contained
- No horizontal layers (services/, utils/)
- Features live together

**Apply Screaming Architecture** (see [SCREAMING_ARCHITECTURE.md](SCREAMING_ARCHITECTURE.md))
- Names reveal intent
- Strangers can understand the system from folder names

---

### 3. Spoke Inventory

**List all spokes needed:**

| Spoke | Type | Priority | Dependencies |
|-------|------|----------|--------------|
| `initialize_capability` | CMD | P0 (skeleton) | None |
| `announce_capability` | CMD | P1 | initialize |
| `capability_announced_to_capabilities` | PRJ | P1 | announce |
| `find_capability` | QRY | P1 | PRJ |
| ... | ... | ... | ... |

**Identify the Walking Skeleton spoke** — the simplest end-to-end slice.

---

### 4. Interface Design

**Define contracts:**

**Commands (Input)**
```erlang
#{
    field1 => binary(),
    field2 => integer(),
    ...
}
```

**Events (Internal)**
```erlang
#{
    field1 => binary(),
    timestamp => integer(),
    ...
}
```

**FACTs (Mesh Output)**
```erlang
#{
    mri => binary(),
    data => map(),
    published_at => integer()
}
```

**Query APIs**
```
GET /api/{resource}/:id
GET /api/{resource}?filter=value
```

---

### 5. Technology Decisions

**Document choices:**

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Language | Erlang/OTP | BEAM ecosystem, fault tolerance |
| Event Store | ReckonDB | Embedded, event-sourcing native |
| Read Models | SQLite | Embedded, fast queries |
| Mesh | Macula | QUIC transport, existing infra |

**Record alternatives considered and why rejected.**

---

### 6. Implementation Planning

**Order the work:**

1. **Iteration 0: Walking Skeleton**
   - Scaffold codebase
   - CI/CD pipeline
   - GitOps setup
   - `initialize_*` spoke end-to-end
   - Deploy to all environments

2. **Iteration 1: Core Feature**
   - Primary business spoke(s)
   - Essential projections
   - Basic queries

3. **Iteration N: Additional Features**
   - Secondary spokes
   - Advanced queries
   - Integrations

**See:** [HECATE_WALKING_SKELETON.md](HECATE_WALKING_SKELETON.md)

---

### 7. Risk Mitigation Design

**Address risks from AnD:**

| Risk | Mitigation in Design |
|------|---------------------|
| Performance | Design indexes, plan for caching |
| Scalability | Stateless handlers, partitioned streams |
| Complexity | Smaller spokes, clear boundaries |
| Integration | Define interfaces early, mock dependencies |

---

## Outputs

### Required

- [ ] **Domain Model** — Dossiers, events, commands mapped
- [ ] **Architecture Diagram** — Departments, spokes, flows
- [ ] **Spoke Inventory** — All spokes with priorities
- [ ] **PLAN_*.md** — Implementation plan document

### Recommended

- [ ] **Interface Specifications** — Command/Event/FACT schemas
- [ ] **Technology Decisions** — Choices with rationale
- [ ] **Iteration Plan** — Walking skeleton + subsequent iterations

---

## PLAN_*.md Template

```markdown
# PLAN: {Domain Name}

**Status:** Planning | In Progress | Complete
**Created:** {date}

## Overview

{Brief description of what we're building}

## Domain Model

### Dossiers

| Dossier | Stream ID Pattern | Description |
|---------|------------------|-------------|
| ... | ... | ... |

### Spokes (CMD)

| Spoke | Events | Priority |
|-------|--------|----------|
| ... | ... | ... |

### Projections (PRJ)

| Projection | Source Event | Target Table |
|------------|--------------|--------------|
| ... | ... | ... |

### Queries (QRY)

| Query | Parameters | Returns |
|-------|------------|---------|
| ... | ... | ... |

## Implementation Phases

### Phase 0: Walking Skeleton
- [ ] Scaffold
- [ ] CI/CD
- [ ] GitOps
- [ ] `initialize_*` spoke

### Phase 1: Core
- [ ] ...

## Open Questions

- ...

## Decisions Log

| Date | Decision | Rationale |
|------|----------|-----------|
| ... | ... | ... |
```

---

## Checklists

### Before Starting AnP

- [ ] AnD outputs available
- [ ] Problem understood
- [ ] Requirements clear
- [ ] Constraints known

### Before Leaving AnP

- [ ] Domain modeled (dossiers, spokes, events)
- [ ] Architecture documented
- [ ] Spokes inventoried with priorities
- [ ] Walking skeleton identified
- [ ] PLAN_*.md written
- [ ] Ready to implement

---

## Anti-Patterns

| Anti-Pattern | Problem | Instead |
|--------------|---------|---------|
| **Big design up front** | Over-engineering | Design for first iterations, evolve |
| **Horizontal thinking** | services/, handlers/ | Vertical spokes |
| **Skipping the skeleton** | Code first, deploy later | Walking skeleton first |
| **Implicit decisions** | "We just knew" | Document decisions |
| **Ignoring constraints** | Beautiful but impossible | Design within constraints |

---

## Transition to InT

When AnP is complete:

1. Review architecture with team
2. Confirm walking skeleton scope
3. Ensure CI/CD requirements known
4. Proceed to [HECATE_InT](HECATE_InT.md)

---

*Design the system. Plan the work. Then build.* 🗝️
