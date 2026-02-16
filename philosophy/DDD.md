# DDD.md — The Dossier Principle

*Process-centric domain modeling for Division architecture.*

---

## The Two Mental Models

Most Event-Sourced systems approach aggregates from a **data-centric** perspective. Division Architecture approaches aggregates from a **process-centric** perspective.

### Data-Centric (Traditional)

```
Aggregate = object with current state
Events = mutations applied to the object
Question: "What IS this thing right now?"
```

```erlang
%% Traditional: Aggregate is a data structure
-record(capability_aggregate, {
    mri,
    status,        % current state
    announced_at,
    updated_at
}).

%% Events "apply" to mutate state
apply_event(Agg, #capability_announced_v1{} = E) ->
    Agg#capability_aggregate{
        mri = E#capability_announced_v1.mri,
        status = active,
        announced_at = E#capability_announced_v1.timestamp
    }.
```

The aggregate is a **thing**. Events happen **to** it.

### Process-Centric (Dossier)

```
Aggregate = the dossier itself (ordered event slips)
Desks = stations the dossier passes through
Question: "What has HAPPENED to this dossier?"
```

```
Dossier: capability-mri:capability:io.macula/weather
├── [slip] capability_announced_v1    ← added at announce_capability desk
├── [slip] capability_updated_v1      ← added at update_capability desk
├── [slip] capability_endorsed_v1     ← added at endorse_capability desk
└── [slip] capability_revoked_v1      ← added at revoke_capability desk
```

The dossier **is** its history. There is no separate "state object."

---

## The Dossier Metaphor

Imagine a physical folder (dossier) moving through an office:

1. **Dossier arrives** at a desk
2. **Clerk reviews** the slips inside (events so far)
3. **Clerk may add** a new slip (new event)
4. **Dossier moves on** to the next desk

Each desk has a specific responsibility:
- The `announce_capability` desk handles new announcements
- The `update_capability` desk handles modifications
- The `revoke_capability` desk handles revocations

The dossier accumulates slips as it moves through the process.

---

## Consequences of the Dossier Model

### 1. Stream ID = Dossier Identity

The event stream ID is the dossier's unique identifier:

```
capability-mri:capability:io.macula/weather
reputation-did:macula:agent123
subscription-{subscriber}-{publisher}
```

All slips (events) for a dossier share this stream ID.

### 2. Desks Are Verbs, Not Nouns

Desks represent **process steps**, not entities:

| ❌ Data-Centric (Noun) | ✅ Process-Centric (Verb) |
|------------------------|---------------------------|
| `capability/` | `announce_capability/` |
| `reputation/` | `track_rpc_call/` |
| `subscription/` | `subscribe_to_agent/` |

Each desk is a station where a specific action can occur.

### 3. "Rebuilding State" = Reading the Dossier

To understand current state, read the dossier front-to-back:

```erlang
%% Not "applying mutations" — just reading slips
get_current_status(DossierId) ->
    Slips = reckon_evoq:read_stream(DossierId),
    lists:foldl(fun interpret_slip/2, #{}, Slips).

interpret_slip(#capability_announced_v1{}, Acc) ->
    Acc#{status => active};
interpret_slip(#capability_revoked_v1{}, Acc) ->
    Acc#{status => revoked}.
```

### 4. Business Rules = "Can This Slip Be Added?"

Validation is about whether a new slip can be added to this dossier:

```erlang
%% Handler checks: given the slips so far, can we add this one?
maybe_announce_capability(Cmd, Dossier) ->
    case dossier_status(Dossier) of
        empty -> 
            %% New dossier, can add announcement slip
            {ok, capability_announced_v1:from_cmd(Cmd)};
        active -> 
            %% Already has announcement slip
            {error, already_announced};
        revoked -> 
            %% Has revocation slip
            {error, capability_revoked}
    end.
```

### 5. Projections = "Index Cards" for the Filing Cabinet

Projections create index cards (read models) so you can find dossiers without opening every one:

```
Filing Cabinet (Event Store)
├── Dossier: capability-mri:...weather
├── Dossier: capability-mri:...forecast
└── Dossier: capability-mri:...alerts

Index Cards (Projections/Read Models)
├── By Agent: [weather, forecast] → agent123
├── By Tag: [weather, alerts] → "weather"
└── By Status: [weather, forecast, alerts] → "active"
```

---

## Domain Design with Dossiers

When designing a domain, ask:

### 1. What dossiers exist?

What are the "things" that accumulate history?

- Capabilities (announced, updated, revoked)
- Reputations (calls tracked, disputes flagged)
- Subscriptions (created, cancelled)
- Identities (created, paired, updated)

### 2. What desks process each dossier?

What actions can happen to each dossier type?

```
Capability Dossier passes through:
├── announce_capability (creates dossier, adds first slip)
├── update_capability (adds update slip)
├── endorse_capability (adds endorsement slip)
└── revoke_capability (adds revocation slip)
```

### 3. What slips (events) can be added?

Each desk adds specific slip types:

| Desk | Slip (Event) |
|--------------|--------------|
| `announce_capability` | `capability_announced_v1` |
| `update_capability` | `capability_updated_v1` |
| `endorse_capability` | `capability_endorsed_v1` |
| `revoke_capability` | `capability_revoked_v1` |

### 4. What index cards (projections) do we need?

How will we find dossiers without opening each one?

- Find capabilities by agent
- Find capabilities by tag
- Find active capabilities
- List top-rated agents (reputation index)

---

## Example: Designing the Reputation Domain

### Dossier: Agent Reputation

Stream ID: `reputation-{agent_id}`

### Desks:

```
reputation dossier passes through:
├── track_rpc_call      → adds rpc_call_tracked_v1 slip
├── flag_dispute        → adds dispute_flagged_v1 slip
├── resolve_dispute     → adds dispute_resolved_v1 slip
└── award_badge         → adds badge_awarded_v1 slip
```

### Slips (Events):

```erlang
rpc_call_tracked_v1     % caller, callee, procedure, success, latency
dispute_flagged_v1      % reporter, reason, evidence
dispute_resolved_v1     % resolver, resolution, outcome
badge_awarded_v1        % badge_type, criteria_met
```

### Index Cards (Projections):

```sql
-- Reputation scores (aggregated from slips)
CREATE TABLE reputation (
    agent_id TEXT PRIMARY KEY,
    score REAL,
    total_calls INTEGER,
    success_rate REAL
);

-- Dispute history
CREATE TABLE disputes (
    id TEXT PRIMARY KEY,
    agent_id TEXT,
    status TEXT,
    ...
);

-- Badges earned
CREATE TABLE badges (
    agent_id TEXT,
    badge_type TEXT,
    awarded_at INTEGER
);
```

---

## The Dossier Checklist

When creating a new domain:

- [ ] **Identify the dossier** — What accumulates history?
- [ ] **Define the stream ID pattern** — How is each dossier uniquely identified?
- [ ] **List the desks** — What actions/process steps exist?
- [ ] **Define the slips (events)** — What gets added at each desk?
- [ ] **Design the index cards (projections)** — How will we query?
- [ ] **Map the process flow** — Which desks can the dossier visit, in what order?

---

## Why This Matters

The dossier model:

1. **Aligns with business reality** — Processes ARE dossiers moving through departments
2. **Makes desks obvious** — Each desk is a process step
3. **Clarifies validation** — "Can this slip be added to this dossier?"
4. **Simplifies projections** — "Index cards for the filing cabinet"
5. **Enables code generation** — The pattern is mechanical, not creative

---

*The dossier is the aggregate. The slips are the events. The desks are the capabilities.*

*Pass the dossier. Add the slip. Move on.* 🗝️
