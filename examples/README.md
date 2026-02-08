# Hecate Examples Library

*Canonical examples for training and documentation.*

---

## Purpose

This directory contains well-documented examples of correct patterns used in the Hecate ecosystem. These examples serve as:

1. **Training data** for fine-tuning Ollama models
2. **Reference implementations** for developers
3. **Teaching materials** for onboarding

---

## Examples Index

| Example | Pattern | Key Concepts |
|---------|---------|--------------|
| [PARENT_CHILD_AGGREGATES.md](PARENT_CHILD_AGGREGATES.md) | Torch → Cartwheel | Parent identifies, child initiates; event stream ownership |
| [VERTICAL_API_HANDLERS.md](VERTICAL_API_HANDLERS.md) | API in spokes | Handlers in spokes, shared utilities, dependency management |

---

## Example Structure

Each example should include:

1. **The Pattern** - What we're teaching
2. **Wrong Way** - Antipattern to avoid
3. **Correct Way** - The proper implementation
4. **Code Examples** - Working Erlang/Elixir code
5. **Key Takeaways** - Summary points
6. **Training Note** - What the model should learn

---

## Adding Examples

When adding a new example:

1. Create `EXAMPLE_NAME.md` in this directory
2. Follow the structure above
3. Include complete, runnable code snippets
4. Add entry to this README's index
5. Tag with date and origin

---

## Future Examples (TODO)

- [ ] Event stream patterns (stream-per-aggregate vs stream-per-type)
- [ ] Projection patterns (SQLite read models)
- [ ] Mesh integration (Emitter/Listener/Requester/Responder)
- [ ] Process manager patterns
- [ ] Bit flag status fields
- [ ] CQRS query optimization

---

## Usage for Fine-Tuning

These examples can be converted to training data format:

```python
# Example conversion for fine-tuning
{
    "prompt": "How should API handlers be organized in a vertical slice architecture?",
    "completion": "<content from VERTICAL_API_HANDLERS.md>"
}
```

The examples are written to be self-contained and teachable.

---

*Maintained by: Hecate Team*
*Last updated: 2026-02-08*
