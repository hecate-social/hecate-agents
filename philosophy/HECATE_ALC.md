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

## Phase 1: AnD — Analysis & Discovery

*Understand the problem before solving it.*

### Purpose

- Understand what needs to be built
- Discover constraints and requirements
- Explore the problem space
- Identify unknowns and risks

### Activities

| Activity | Description |
|----------|-------------|
| **Requirements gathering** | What does the user/system need? |
| **Domain exploration** | What concepts exist? What language does the domain use? |
| **Constraint identification** | Technical limits, business rules, time/budget |
| **Risk assessment** | What could go wrong? What's unknown? |
| **Prior art research** | What exists already? What can we learn from? |
| **Stakeholder interviews** | Who cares about this? What do they need? |

### Outputs

- Problem statement
- Requirements list (functional & non-functional)
- Domain glossary
- Constraints document
- Risk register
- Research notes

### Agent Approach

```
"Before I write any code, I need to understand:
 - What problem am I solving?
 - Who am I solving it for?
 - What constraints exist?
 - What could go wrong?"
```

### Anti-Patterns

- ❌ Jumping to code without understanding the problem
- ❌ Assuming requirements are complete
- ❌ Ignoring constraints until they bite
- ❌ Skipping domain understanding

---

## Phase 2: AnP — Architecture & Planning

*Design the solution before building it.*

### Purpose

- Design the system structure
- Plan the implementation approach
- Define the dossiers, spokes, and flows
- Establish technical decisions

### Activities

| Activity | Description |
|----------|-------------|
| **Domain modeling** | Identify dossiers, events, commands |
| **Architecture design** | Define departments (CMD/PRJ/QRY), spokes |
| **Interface design** | APIs, mesh topics, data contracts |
| **Technology selection** | Languages, frameworks, infrastructure |
| **Task breakdown** | Split work into implementable chunks |
| **Dependency mapping** | What depends on what? What order? |

### Outputs

- Domain model (dossiers, events, commands)
- Architecture diagram
- Spoke inventory
- API/interface specifications
- Technology decisions document
- Implementation plan (ordered tasks)
- PLAN_*.md document

### Agent Approach

```
"Before I implement, I need to design:
 - What dossiers exist?
 - What spokes process them?
 - What events flow through the system?
 - What's the implementation order?"
```

### The Dossier Question

For each domain, ask:
1. What dossiers exist? (things that accumulate history)
2. What desks process each dossier? (spokes)
3. What slips can be added? (events)
4. What index cards do we need? (projections)

### Anti-Patterns

- ❌ Starting implementation without a plan
- ❌ Designing horizontally (by technical layer)
- ❌ Ignoring the dossier principle
- ❌ Over-engineering before validating

---

## Phase 3: InT — Implementation & Testing

*Build it right.*

### Purpose

- Implement the designed solution
- Test as you build
- Follow the architecture strictly
- Produce working, verified code

### Activities

| Activity | Description |
|----------|-------------|
| **Spoke implementation** | Build each spoke following CODEGEN templates |
| **Unit testing** | Test handlers, projections, queries |
| **Integration testing** | Test spoke interactions, mesh flows |
| **Code review** | Check for antipatterns, doctrine violations |
| **Documentation** | Code comments, API docs, README updates |
| **Refactoring** | Clean up as patterns emerge |

### Outputs

- Working code (spokes, projections, queries)
- Test suites
- Updated documentation
- Verified builds (compile, dialyzer, tests pass)

### Agent Approach

```
"When implementing, I follow the templates:
 - One spoke at a time
 - Test as I build
 - Check for antipatterns
 - Compile and verify frequently"
```

### The Implementation Loop

```
For each spoke in plan:
    1. Generate from CODEGEN template
    2. Fill in business logic
    3. Write tests
    4. Verify (compile, dialyzer, eunit)
    5. Check against ANTIPATTERNS
    6. Commit
```

### Anti-Patterns

- ❌ Implementing without tests
- ❌ Deviating from the architecture
- ❌ Creating horizontal structures (services/, utils/)
- ❌ Skipping verification steps
- ❌ Large commits with many changes

---

## Phase 4: DoO — Deployment & Operations

*Ship it and keep it running.*

### Purpose

- Deploy to production
- Monitor and observe
- Respond to issues
- Iterate based on feedback

### Activities

| Activity | Description |
|----------|-------------|
| **Release preparation** | Version, changelog, release notes |
| **Deployment** | Push to production (GitOps, CI/CD) |
| **Monitoring setup** | Metrics, logs, alerts |
| **Smoke testing** | Verify production deployment |
| **Incident response** | Handle issues as they arise |
| **Feedback collection** | Gather user feedback, metrics |
| **Iteration planning** | What's next? Back to AnD |

### Outputs

- Deployed release
- Monitoring dashboards
- Runbooks
- Incident reports
- Feedback log
- Next iteration backlog

### Agent Approach

```
"After deployment, I observe:
 - Is it working as expected?
 - What are users experiencing?
 - What needs improvement?
 - What did I learn for next time?"
```

### Anti-Patterns

- ❌ Deploy and forget
- ❌ No monitoring or observability
- ❌ Ignoring production feedback
- ❌ Skipping release documentation

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

**Gate:** Problem is understood
- [ ] Requirements documented
- [ ] Domain concepts identified
- [ ] Constraints known
- [ ] Risks assessed

### AnP → InT

**Gate:** Solution is designed
- [ ] Dossiers defined
- [ ] Spokes identified
- [ ] Architecture documented
- [ ] Tasks ordered

### InT → DoO

**Gate:** Solution is built
- [ ] All spokes implemented
- [ ] Tests passing
- [ ] Dialyzer clean
- [ ] Documentation updated

### DoO → AnD (next cycle)

**Gate:** Feedback collected
- [ ] Production stable
- [ ] Metrics gathered
- [ ] User feedback collected
- [ ] Next priorities identified

---

## TUI Integration

The Hecate TUI provides views for each phase:

| Phase | TUI View | Purpose |
|-------|----------|---------|
| AnD | Analysis & Discovery | Research, requirements, domain exploration |
| AnP | Architecture & Planning | Design, modeling, planning |
| InT | Implementation & Testing | Coding, testing, verification |
| DoO | Deployment & Operations | Release, monitoring, operations |

Navigate between phases as the project progresses.

---

## For Agents

When working on a project:

1. **Know which phase you're in** — Don't implement during AnD, don't design during InT
2. **Complete the phase** — Finish phase outputs before moving on
3. **Respect the gates** — Check transition criteria
4. **Document as you go** — Each phase produces artifacts
5. **Cycle fast** — Small iterations beat big bang

---

## Summary

| Phase | Question | Output |
|-------|----------|--------|
| **AnD** | What problem? | Requirements, domain, constraints |
| **AnP** | What solution? | Architecture, plan, tasks |
| **InT** | Does it work? | Code, tests, docs |
| **DoO** | Is it running? | Deployment, monitoring, feedback |

---

*The wheel turns. Each phase feeds the next. The goddess guides the cycle.* 🔥🗝️🔥
