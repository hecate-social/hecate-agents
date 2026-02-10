# Example: Parent-Child Aggregate Pattern

*Canonical example: Torch → Cartwheel relationship*

---

## The Pattern

When a parent aggregate (Torch) needs to create child aggregates (Cartwheels):

1. **Parent IDENTIFIES** children (declares "I need X")
2. **Child INITIATES** itself (starts its own lifecycle)

This separates:
- **What exists** (parent's decision)
- **How it works** (child's lifecycle)

---

## Domain Model

```
Torch (Business Endeavor)
├── Has 0..N Cartwheels (Bounded Contexts)
├── IDENTIFIES what cartwheels it needs
└── Does NOT control cartwheel lifecycle

Cartwheel (Bounded Context)
├── Belongs to one Torch
├── INITIATES its own lifecycle
└── Manages its own phases (DnA, AnP, TnI, DnO)
```

---

## Correct Flow

```
User: "Create a torch with a cartwheel for user auth"

1. POST /api/torch/initiate
   → torch_initiated_v1 stored in Torch's stream
   → torch_initiated_v1 emitted to mesh (optional)

2. POST /api/torches/:id/cartwheels/identify
   → cartwheel_identified_v1 stored in Torch's stream
   → cartwheel_identified_v1 emitted to mesh

3. Cartwheel service listener receives fact
   → Policy decides to initiate
   → initiate_cartwheel_v1 command dispatched
   → cartwheel_initiated_v1 stored in Cartwheel's stream
```

---

## Wrong Flow (Antipattern)

```
❌ torch_initiated_v1 → automatically create cartwheel
```

**Why wrong:**
- Assumes 1:1 relationship
- No explicit decision about what cartwheels needed
- Child creation happens without parent consent
- Can't have torches with 0, 2, or 10 cartwheels

---

## Code Structure

### manage_torches (Parent Domain)

```
apps/manage_torches/src/
├── initiate_torch/
│   ├── initiate_torch_v1.erl            # Command
│   ├── torch_initiated_v1.erl           # Event
│   ├── maybe_initiate_torch.erl         # Handler
│   └── torch_initiated_v1_to_mesh.erl   # Emitter (optional)
│
└── identify_cartwheel/                   # Parent identifies children
    ├── identify_cartwheel_v1.erl        # Command
    ├── cartwheel_identified_v1.erl      # Event (in Torch stream!)
    ├── maybe_identify_cartwheel.erl     # Handler
    └── cartwheel_identified_v1_to_mesh.erl  # Emitter → mesh
```

### manage_cartwheels (Child Domain)

```
apps/manage_cartwheels/src/
└── initiate_cartwheel/
    ├── initiate_cartwheel_v1.erl        # Command
    ├── cartwheel_initiated_v1.erl       # Event (in Cartwheel stream!)
    ├── maybe_initiate_cartwheel.erl     # Handler
    │
    │ # Listener + Policy live IN this spoke (vertical slice)
    ├── initiate_cartwheel_spoke_sup.erl
    ├── subscribe_to_cartwheel_identified.erl           # Listener
    └── on_cartwheel_identified_maybe_initiate_cartwheel.erl  # Policy
```

---

## Event Stream Ownership

| Event | Stored In | Reason |
|-------|-----------|--------|
| `torch_initiated_v1` | Torch stream | Birth of torch |
| `cartwheel_identified_v1` | Torch stream | Parent's decision |
| `cartwheel_initiated_v1` | Cartwheel stream | Birth of cartwheel |

**Key insight:** `cartwheel_identified_v1` belongs to the torch because it's the parent's decision about what children exist.

---

## Semantic Distinction

| Action | Owner | Verb | Meaning |
|--------|-------|------|---------|
| **Identify** | Parent | "I need X" | Parent declares what children exist |
| **Initiate** | Child | "I exist" | Child starts its lifecycle |

---

## API Endpoints

```http
# Torch endpoints
POST /api/torch/initiate           # Create a torch
GET  /api/torches                  # List all torches
GET  /api/torches/:torch_id        # Get specific torch

# Cartwheel identification (on Torch!)
POST /api/torches/:torch_id/cartwheels/identify

# Cartwheel lifecycle (separate domain)
GET  /api/cartwheels               # List all cartwheels
GET  /api/cartwheels/:id           # Get specific cartwheel
```

---

## Listener Placement Rule

The listener (`subscribe_to_cartwheel_identified`) lives **inside** the `initiate_cartwheel/` spoke, NOT as a separate spoke.

**Why?** Its sole purpose is to trigger cartwheel initiation. Vertical slicing means the spoke owns everything it needs.

```
❌ WRONG:
manage_cartwheels/src/
├── subscribe_to_cartwheel_identified/   # Separate spoke
└── initiate_cartwheel/                  # Another spoke

✅ CORRECT:
manage_cartwheels/src/
└── initiate_cartwheel/
    ├── ...command, event, handler...
    ├── subscribe_to_cartwheel_identified.erl   # IN the spoke
    └── on_cartwheel_identified_maybe_initiate_cartwheel.erl
```

---

## Process Manager Naming

```
on_{source_event}_{action}_{target}
```

Example: `on_cartwheel_identified_maybe_initiate_cartwheel`

- **Source:** `cartwheel_identified` (from manage_torches)
- **Action:** `maybe_initiate` (policy decision)
- **Target:** `cartwheel` (in manage_cartwheels)

---

## Complete Erlang Example

### identify_cartwheel_v1.erl (Command)

```erlang
-module(identify_cartwheel_v1).
-export([new/1, validate/1, to_map/1, from_map/1]).

-record(identify_cartwheel_v1, {
    torch_id      :: binary(),
    context_name  :: binary(),
    description   :: binary() | undefined,
    identified_by :: binary() | undefined
}).

new(#{torch_id := TorchId, context_name := ContextName} = Params) ->
    {ok, #identify_cartwheel_v1{
        torch_id = TorchId,
        context_name = ContextName,
        description = maps:get(description, Params, undefined),
        identified_by = maps:get(identified_by, Params, undefined)
    }};
new(_) ->
    {error, missing_required_fields}.
```

### cartwheel_identified_v1.erl (Event)

```erlang
-module(cartwheel_identified_v1).
-export([new/1, to_map/1, from_map/1]).

-record(cartwheel_identified_v1, {
    torch_id      :: binary(),
    cartwheel_id  :: binary(),
    context_name  :: binary(),
    description   :: binary() | undefined,
    identified_by :: binary() | undefined,
    identified_at :: non_neg_integer()
}).

new(#{torch_id := TorchId, context_name := ContextName} = Params) ->
    CartwheelId = maps:get(cartwheel_id, Params, generate_id()),
    {ok, #cartwheel_identified_v1{
        torch_id = TorchId,
        cartwheel_id = CartwheelId,
        context_name = ContextName,
        description = maps:get(description, Params, undefined),
        identified_by = maps:get(identified_by, Params, undefined),
        identified_at = erlang:system_time(millisecond)
    }}.
```

### Policy (Clean Code)

```erlang
-module(on_cartwheel_identified_maybe_initiate_cartwheel).
-export([handle/1]).

handle(FactData) ->
    Params = extract_params(FactData),
    Result = do_initiate(Params),
    log_result(maps:get(cartwheel_id, Params), Result),
    Result.

extract_params(FactData) ->
    #{
        cartwheel_id => get_field(cartwheel_id, FactData),
        torch_id => get_field(torch_id, FactData),
        context_name => get_field(context_name, FactData),
        description => get_field(description, FactData)
    }.

do_initiate(Params) ->
    with_command(initiate_cartwheel_v1:new(Params)).

with_command({ok, Cmd}) -> dispatch(maybe_initiate_cartwheel:dispatch(Cmd));
with_command({error, _} = E) -> E.

dispatch({ok, _, _}) -> ok;
dispatch({error, _} = E) -> E.
```

---

## Key Takeaways

1. **Parent identifies, child initiates** - Clear separation of concerns
2. **Events belong to aggregate that makes decision** - `cartwheel_identified` in torch stream
3. **Listeners live in spoke they trigger** - Vertical slicing
4. **No auto-creation** - Explicit decisions about what exists
5. **Policy contains "maybe"** - Conditional logic is explicit

---

## Training Note

This example can be used for fine-tuning to teach:
- Parent-child aggregate relationships
- Event stream ownership
- Vertical slice organization
- Listener placement rules
- Process manager naming conventions

*Date: 2026-02-08*
*Origin: Hecate Walking Skeleton implementation*
