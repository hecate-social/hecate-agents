# SCREAMING_ARCHITECTURE.md — Names That Reveal Intent

*Your code structure should scream what the system does, not what frameworks you used.*

---

## The Stranger Test

Imagine a stranger opens your codebase for the first time.

**Question:** Can they tell what the system does just by reading directory and file names?

```
❌ FAILS THE STRANGER TEST
src/
├── controllers/
├── services/
├── repositories/
├── models/
└── utils/

# "It's... some kind of web app? Maybe?"
```

```
✅ PASSES THE STRANGER TEST
src/
├── announce_capability/
├── revoke_capability/
├── track_rpc_call/
├── flag_dispute/
└── resolve_dispute/

# "This system manages capabilities and handles disputes."
```

The structure itself is documentation.

---

## The Core Principle

> **Names should scream WHAT the code does, not HOW it does it.**

| Screams HOW (Technical) | Screams WHAT (Business) |
|------------------------|------------------------|
| `handler` | `announce_capability` |
| `service` | `track_rpc_call` |
| `manager` | `resolve_dispute` |
| `processor` | `detect_llm_models` |
| `worker` | `listen_for_llm_request` |
| `controller` | `grant_capability` |

---

## Naming Rules

### 1. Directories = Verb Phrases

Slice/spoke directories are named as **verb + noun**:

```
✅ GOOD
announce_capability/
track_rpc_call/
flag_dispute/
grant_capability/

❌ BAD
capability/
rpc/
dispute/
handler/
```

The directory name IS the action.

### 2. Files = Role in the Slice

Files within a slice follow consistent patterns:

| Pattern | Role | Example |
|---------|------|---------|
| `{command}_v1.erl` | Command record | `announce_capability_v1.erl` |
| `{event}_v1.erl` | Event record | `capability_announced_v1.erl` |
| `maybe_{command}.erl` | Handler | `maybe_announce_capability.erl` |
| `{command}_responder_v1.erl` | HOPE receiver | `announce_capability_responder_v1.erl` |
| `{event}_to_mesh.erl` | Emitter | `capability_announced_to_mesh.erl` |
| `{command}_spoke_sup.erl` | Supervisor | `announce_capability_spoke_sup.erl` |

### 3. Events = Past Tense

Events describe **what happened**:

```
✅ GOOD
capability_announced_v1
rpc_call_tracked_v1
dispute_flagged_v1
badge_awarded_v1

❌ BAD
capability_event
announce_event
new_capability
capability_created  # CRUD is not business language
```

### 4. No Technical Suffixes

Avoid generic technical suffixes that add no meaning:

```
❌ BANNED SUFFIXES
*_handler
*_manager
*_processor
*_worker
*_service
*_helper
*_util
*_impl
```

If you need a suffix, use one that describes the **role in the architecture**:

```
✅ ALLOWED SUFFIXES (with meaning)
*_v1              # Version
*_spoke_sup       # Spoke supervisor
*_responder_v1    # HOPE receiver
*_to_mesh         # Emitter to mesh
*_to_{table}      # Projection to table
*_store           # Storage accessor
```

---

## The Speak-Aloud Test

Read your names aloud. Do they make sense as English sentences?

```
✅ GOOD (reads naturally)
"This slice announces capabilities"
"This handler maybe announces a capability"
"This emitter sends capability_announced to mesh"

❌ BAD (reads like code gibberish)
"This handler handles capability"
"This processor processes events"
"This manager manages things"
```

---

## Domain-Driven Naming

Names should come from the **business domain**, not the technical implementation:

| Domain Term | Use It | Don't Use |
|-------------|--------|-----------|
| Capability | `announce_capability` | `create_capability` |
| Reputation | `track_rpc_call` | `log_call` |
| Dispute | `flag_dispute` | `create_dispute` |
| Badge | `award_badge` | `add_badge` |

**Talk to the business.** Use their language.

CRUD verbs (`create`, `read`, `update`, `delete`) are technical. Business verbs (`announce`, `grant`, `revoke`, `flag`, `resolve`, `award`) are meaningful.

---

## Examples: Before and After

### Example 1: LLM Service

```
❌ BEFORE (Technical)
src/
├── llm_handler.erl
├── llm_poller.erl
├── llm_listener.erl
└── llm_publisher.erl

✅ AFTER (Screaming)
src/
├── detect_llm_models/
│   └── detect_llm_models.erl
├── listen_for_llm_request/
│   └── listen_for_llm_request.erl
└── announce_llm_availability/
    └── announce_llm_availability_to_mesh.erl
```

### Example 2: Subscription Domain

```
❌ BEFORE (Technical)
src/
├── subscription_service.erl
├── subscription_handler.erl
├── subscription_repo.erl
└── subscription_events.erl

✅ AFTER (Screaming)
src/
├── subscribe_to_agent/
├── unsubscribe_from_agent/
├── record_subscriber/
└── notify_subscribers/
```

### Example 3: Reputation Domain

```
❌ BEFORE (Technical)
src/
├── reputation_manager.erl
├── call_tracker.erl
├── dispute_processor.erl
└── badge_handler.erl

✅ AFTER (Screaming)
src/
├── track_rpc_call/
├── flag_dispute/
├── resolve_dispute/
└── award_badge/
```

---

## The Architecture as Documentation

When names scream intent:

1. **READMEs become optional** — The structure explains itself
2. **Onboarding is faster** — New devs navigate by reading folders
3. **Code reviews are easier** — "This change is in `flag_dispute/`, so it's about disputes"
4. **Refactoring is safer** — Clear boundaries = fewer surprises

---

## Common Mistakes

### 1. Using Framework Names

```
❌ BAD
phoenix_controller.erl
cowboy_handler.erl
ecto_repo.erl

# The framework is an implementation detail, not the business
```

### 2. Using Pattern Names

```
❌ BAD
capability_facade.erl
capability_factory.erl
capability_adapter.erl

# Design patterns are implementation, not intent
```

### 3. Using Generic Names

```
❌ BAD
base_handler.erl
common_utils.erl
shared_helpers.erl

# Generic names mean generic (confused) responsibilities
```

---

## The Rule

> **If a stranger can't tell what your system does from the folder names, rename them.**

Your architecture should scream:
- What business capabilities exist
- What actions can be performed
- What events can happen

It should NOT scream:
- What frameworks you used
- What design patterns you applied
- What layers you have

---

## See Also

- [VERTICAL_SLICING.md](VERTICAL_SLICING.md) — Features live together
- [CARTWHEEL.md](CARTWHEEL.md) — The full architecture
- [../skills/ANTIPATTERNS.md](../skills/ANTIPATTERNS.md) — Naming demons to avoid

---

*Let the code scream. Let the structure speak. Let the names tell the story.* 🗝️
