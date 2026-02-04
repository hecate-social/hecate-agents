# hecate-agents

*Shaping material for Hecate agent runtimes.*

This repository contains the philosophical foundations, mental models, skills, and guardrails that shape how Hecate agents think and work.

---

## Structure

```
hecate-agents/
├── SOUL.md                    # Identity, values, personality
│
├── philosophy/                # Mental models
│   ├── DDD.md                 # The Dossier Principle
│   └── CARTWHEEL.md           # Cartwheel Architecture overview
│
├── skills/                    # Executable knowledge
│   ├── ANTIPATTERNS.md        # What NOT to do (guardrails)
│   └── codegen/               # Code generation templates
│       └── erlang/
│           └── CODEGEN_ERLANG_EVOQ.md
│
└── guides/                    # Detailed reference (optional)
```

---

## Layers

| Layer | Purpose | Files |
|-------|---------|-------|
| **Soul** | Identity, personality, values | `SOUL.md` |
| **Philosophy** | Mental models, principles | `philosophy/*.md` |
| **Skills** | Executable knowledge, templates | `skills/**/*.md` |
| **Guardrails** | What NOT to do | `skills/ANTIPATTERNS.md` |

---

## Usage

### For Apprentices (Claude/AI assistants)

Reference these docs in your workspace `CLAUDE.md`:

```bash
cat ~/work/github.com/hecate-social/hecate-agents/philosophy/DDD.md
cat ~/work/github.com/hecate-social/hecate-agents/skills/ANTIPATTERNS.md
```

### For Hecate TUI

Skills are injected contextually based on the current task:
- Architecture work → Load `philosophy/DDD.md`, `philosophy/CARTWHEEL.md`
- Code generation → Load `skills/codegen/erlang/CODEGEN_ERLANG_EVOQ.md`
- Code review → Load `skills/ANTIPATTERNS.md`

---

## Contributing

These documents shape how agents think. Changes should be deliberate.

- **Philosophy** changes affect mental models
- **Skills** changes affect code output
- **Guardrails** changes affect quality control

---

*The goddess shapes her servants.* 🔥🗝️🔥
