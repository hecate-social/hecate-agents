# CODE_QUALITY.md — Implementation Standards

*Rules for clean, maintainable code across all languages.*

---

## 🚫 NO NESTED CONTROL STRUCTURES

**Date:** 2026-02-08
**Severity:** STRONG RULE
**Applies to:** Erlang, Elixir, Go, all languages

### The Antipattern

Nested `case`, `if`, `cond`, or other control structures:

```erlang
%% ❌ BAD: 3 nested case statements
handle(FactData) ->
    case extract_params(FactData) of
        {ok, Params} ->
            case create_command(Params) of
                {ok, Cmd} ->
                    case dispatch(Cmd) of
                        {ok, _} -> ok;
                        {error, R} -> {error, R}
                    end;
                {error, R} -> {error, R}
            end;
        {error, R} -> {error, R}
    end.
```

**Why it's bad:**
- Hard to read and understand
- Error handling is scattered
- Each nesting level increases cognitive load
- Difficult to test individual pieces
- Arrow anti-pattern in functional code

### The Rule

> **Maximum 1 level of nesting. Transform nested control into function calls.**

### The Solution: Pipeline with Pattern Matching

```erlang
%% ✅ GOOD: Flat pipeline with helper functions
handle(FactData) ->
    Params = extract_params(FactData),
    Result = do_initiate(Params),
    log_result(Params, Result),
    Result.

do_initiate(Params) ->
    with_command(initiate_division_v1:new(Params)).

with_command({ok, Cmd}) -> dispatch(maybe_initiate_division:dispatch(Cmd));
with_command({error, _} = E) -> E.

dispatch({ok, _, _}) -> ok;
dispatch({error, _} = E) -> E.
```

### Techniques to Flatten Code

| Technique | When to Use |
|-----------|-------------|
| **Pattern matching on function heads** | Multiple cases with different handling |
| **Pipeline (`|>` or explicit)** | Sequential transformations |
| **Helper functions** | Complex steps that deserve names |
| **`with` (Elixir)** | Chained operations that may fail |
| **Early returns** | Guard conditions at top of function |

### Elixir Examples

```elixir
# ❌ BAD: Nested case
def handle(data) do
  case validate(data) do
    {:ok, valid} ->
      case process(valid) do
        {:ok, result} ->
          case save(result) do
            {:ok, _} -> :ok
            {:error, r} -> {:error, r}
          end
        {:error, r} -> {:error, r}
      end
    {:error, r} -> {:error, r}
  end
end

# ✅ GOOD: with expression
def handle(data) do
  with {:ok, valid} <- validate(data),
       {:ok, result} <- process(valid),
       {:ok, _} <- save(result) do
    :ok
  end
end

# ✅ ALSO GOOD: Pipeline
def handle(data) do
  data
  |> validate()
  |> then(&process/1)
  |> then(&save/1)
  |> to_result()
end
```

### Go Examples

```go
// ❌ BAD: Nested if
func handle(data Data) error {
    if valid, err := validate(data); err == nil {
        if result, err := process(valid); err == nil {
            if err := save(result); err == nil {
                return nil
            } else {
                return err
            }
        } else {
            return err
        }
    } else {
        return err
    }
}

// ✅ GOOD: Early returns
func handle(data Data) error {
    valid, err := validate(data)
    if err != nil {
        return err
    }

    result, err := process(valid)
    if err != nil {
        return err
    }

    return save(result)
}
```

### The Test

Before committing, ask:
- Can I follow the main flow without indentation?
- Is each step named meaningfully?
- Can I test each step independently?

If you're indenting more than once, **refactor**.

---

## 🚫 NO MAGIC NUMBERS OR STRINGS

```erlang
%% ❌ BAD
handle_status(5) -> ...

%% ✅ GOOD
-define(STATUS_COMPLETED, 5).
handle_status(?STATUS_COMPLETED) -> ...
```

---

## 🚫 NO LONG FUNCTIONS

**Guideline:** Functions over 20-25 lines likely need splitting.

Each function should do ONE thing. If you're explaining multiple steps in comments, those steps should be functions.

---

## 🚫 NO COMMENTED-OUT CODE

Delete it. Git remembers.

```erlang
%% ❌ BAD
%% Old implementation:
%% handle(X) -> do_old_thing(X).

%% New implementation:
handle(X) -> do_new_thing(X).

%% ✅ GOOD
handle(X) -> do_new_thing(X).
```

---

## ✅ DO: Consistent Error Returns

```erlang
%% ✅ GOOD: Consistent {ok, _} / {error, _} pattern
validate(X) ->
    case check(X) of
        true -> {ok, X};
        false -> {error, invalid}
    end.

%% ❌ BAD: Mixed patterns
validate(X) ->
    case check(X) of
        true -> X;           %% Unwrapped!
        false -> {error, invalid}
    end.
```

---

## ✅ DO: Meaningful Names

```erlang
%% ❌ BAD
handle_data(D) ->
    R = process(D),
    save(R).

%% ✅ GOOD
handle_order(OrderData) ->
    ProcessedOrder = validate_and_enrich(OrderData),
    persist_order(ProcessedOrder).
```

---

*Add more rules as we discover them.* 🔧
