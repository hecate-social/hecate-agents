# VERTICAL_SLICING.md — Features Live Together

*Why we organize by business capability, not technical layer.*

---

## The Problem with Horizontal Layers

Traditional codebases organize by **technical concern**:

```
❌ HORIZONTAL (Technical Layers)
src/
├── controllers/
│   ├── capability_controller.erl
│   ├── reputation_controller.erl
│   └── subscription_controller.erl
├── services/
│   ├── capability_service.erl
│   ├── reputation_service.erl
│   └── subscription_service.erl
├── repositories/
│   ├── capability_repo.erl
│   ├── reputation_repo.erl
│   └── subscription_repo.erl
└── models/
    ├── capability.erl
    ├── reputation.erl
    └── subscription.erl
```

To understand "how do we announce a capability?", you must read:
- `controllers/capability_controller.erl`
- `services/capability_service.erl`
- `repositories/capability_repo.erl`
- `models/capability.erl`

**Four directories. Four mental context switches. One feature.**

---

## The Vertical Alternative

Organize by **business capability**:

```
✅ VERTICAL (Business Slices)
src/
├── announce_capability/
│   ├── announce_capability_desk_sup.erl
│   ├── announce_capability_v1.erl
│   ├── capability_announced_v1.erl
│   ├── maybe_announce_capability.erl
│   └── capability_announced_to_mesh.erl
├── update_capability/
│   └── ...
├── revoke_capability/
│   └── ...
└── track_rpc_call/
    └── ...
```

To understand "how do we announce a capability?", you read:
- `announce_capability/`

**One directory. One feature. Everything together.**

---

## The Core Principle

> **Add a feature → Add a folder.**
> **Delete a feature → Delete a folder.**
> **No archaeology required.**

When everything for a feature lives together:
- New developers find code faster
- Changes are localized
- Dependencies are explicit
- Testing is focused

---

## What Belongs in a Slice?

A vertical slice contains **everything** needed for that capability:

| Component | Purpose |
|-----------|---------|
| Supervisor | Owns the slice's processes |
| Command | The request structure |
| Event | What happened |
| Handler | Business logic |
| Responder | Mesh HOPE → Command |
| Emitter | Event → Mesh FACT |
| Policy/PM | Cross-domain triggers |

No shared `services/` folder. No central `handlers/` directory. Each slice is self-contained.

---

## The Forbidden Directories

These directory names are **banned**:

| Directory | Why It's Wrong |
|-----------|----------------|
| `services/` | Where business logic goes to be orphaned |
| `utils/` | Junk drawer of unrelated functions |
| `helpers/` | Same as utils, different name |
| `common/` | If it's common, it's a library |
| `shared/` | Shared by whom? For what? |
| `handlers/` | Handlers belong with their commands |
| `listeners/` | Listeners belong with their domains |
| `managers/` | God modules wearing a mask |

If you feel the urge to create one of these:
1. **Stop**
2. Ask: "Which feature owns this?"
3. Put it there

---

## What About Shared Code?

**Truly shared code** becomes a library (separate OTP app):

```
apps/
├── manage_capabilities/    # Domain
├── query_capabilities/     # Domain
└── hecate_mesh/            # Library (shared infrastructure)
```

Libraries are:
- Generic (not business-specific)
- Stable (rarely change)
- Dependency-free (don't depend on domains)

If it's business-specific, it's not shared — it belongs in a feature.

---

## The Desk as Unit

In Division Architecture, the vertical slice is called a **desk**.

Each desk:
- Has its own supervisor
- Contains all its workers
- Owns its piece of the domain
- Can be deployed independently

```
announce_capability/           ← DESK
├── announce_capability_desk_sup.erl     ← Supervisor
├── announce_capability_v1.erl           ← Command
├── capability_announced_v1.erl          ← Event
├── maybe_announce_capability.erl        ← Handler
├── announce_capability_responder_v1.erl ← Frontdesk
└── capability_announced_to_mesh.erl     ← Emitter
```

The desk is the **unit of change**.

---

## Benefits

### 1. Discoverability

```bash
# "Where is capability announcement handled?"
ls src/
# announce_capability/  ← Right there
```

### 2. Isolation

Changes to `announce_capability` don't touch `revoke_capability`. Each desk is independent.

### 3. Testability

```bash
# Test one feature
rebar3 eunit --module=maybe_announce_capability
```

### 4. Deletability

```bash
# Remove a feature
rm -rf src/announce_capability/
# Done. No orphaned code elsewhere.
```

### 5. Onboarding

New developer: "How does X work?"
Answer: "Read the `X/` directory."

---

## The Rule

> **If you're touching files in more than one directory to add a feature, your architecture is horizontal.**

Vertical slicing means:
- One feature = one place
- All the code = together
- No scattering across layers

---

## Anti-Pattern: The Slow Creep

Horizontal organization often creeps in slowly:

1. "I'll just add a quick utility function to `utils/`"
2. "This handler is used by two features, let me put it in `shared/`"
3. "The validation logic should be centralized in `services/`"

**Each step seems reasonable. The result is chaos.**

Fight the creep. Every time you want to add to a horizontal directory, ask: "Which feature owns this?"

---

## See Also

- [SCREAMING_ARCHITECTURE.md](SCREAMING_ARCHITECTURE.md) — Names that reveal intent
- [CARTWHEEL.md](CARTWHEEL.md) — The Division Architecture
- [DDD.md](DDD.md) — The Dossier Principle

---

*Features live together. Code that changes together stays together.* 🗝️
