# Plan: Hecate ALC User Experience

## Status: DRAFT - Awaiting Approval

**Date:** 2026-02-09
**Scope:** TUI UX for Agent Lifecycle (ALC) workflow

---

## Overview

Design a modal TUI experience for managing Torches (business endeavors) and Cartwheels (bounded contexts) through the Agent Lifecycle phases: Discovery & Analysis (DnA), Architecture & Planning (AnP), Testing & Implementation (TnI), and Deployment & Operations (DnO).

---

## 1. Modal Architecture

Three distinct modes with progressive context:

```
┌──────────────────────────────────────────────────────────────────┐
│                         CHAT MODE                                 │
│  Default, lightweight, no project context                         │
│  Entry: TUI startup (no torch detected)                          │
│  Exit: /torch, /torches                                          │
└──────────────────────────────────────────────────────────────────┘
                              │
                    /torch or /torches
                              ▼
┌──────────────────────────────────────────────────────────────────┐
│                        TORCH MODE                                 │
│  Project-level context, torch selected but no active cartwheel   │
│  Entry: Select torch, or auto-detect from CWD                    │
│  Exit: /chat, Esc (to Chat) or /cartwheel (to Cartwheel)        │
└──────────────────────────────────────────────────────────────────┘
                              │
                    /cartwheel or select
                              ▼
┌──────────────────────────────────────────────────────────────────┐
│                      CARTWHEEL MODE                               │
│  Active work unit, phase-specific behavior                        │
│  Entry: Select or create cartwheel                               │
│  Exit: /back (to Torch), /chat (to Chat)                         │
│                                                                   │
│  Phases: DnA ──► AnP ──► TnI ──► DnO                             │
└──────────────────────────────────────────────────────────────────┘
```

### 1.1 Context Detection on Startup

When TUI starts, detect torch from:

1. **Git remote URL** (preferred) - matches against known torches
2. **`.hecate/torch.json`** in CWD or parent directories (fallback)
3. **No match** - start in Chat mode

```
$ cd ~/work/auth-system
$ hecate-tui

# Detects torch from git remote → auto-enters Torch mode
# "Resuming torch: auth-system"
```

---

## 2. Context Display

### 2.1 Chat Mode (No Header)

```
┌────────────────────────────────────────────────────────────────┐
│ Hecate: How can I help you today?                              │
│                                                                │
│ You: What's the weather like?                                  │
│                                                                │
├────────────────────────────────────────────────────────────────┤
│ 🤖 claude-3.5-sonnet │ ● healthy │ [i] Insert                  │
└────────────────────────────────────────────────────────────────┘
```

### 2.2 Torch Mode (Header Appears)

```
┌────────────────────────────────────────────────────────────────┐
│ 🔥 auth-system                                                 │
├────────────────────────────────────────────────────────────────┤
│ Hecate: This torch has 2 cartwheels. Which one?                │
│                                                                │
├────────────────────────────────────────────────────────────────┤
│ 🤖 claude-3.5-sonnet │ ● healthy │ [i] Insert                  │
└────────────────────────────────────────────────────────────────┘
```

### 2.3 Cartwheel Mode (Full Breadcrumb)

```
┌────────────────────────────────────────────────────────────────┐
│ 🔥 auth-system › 🎡 user-registration › 📍 DnA │ 🤖 claude     │
├────────────────────────────────────────────────────────────────┤
│ Hecate: What authentication method should we use?              │
│                                                                │
├────────────────────────────────────────────────────────────────┤
│ ● healthy │ [i] Insert                                         │
└────────────────────────────────────────────────────────────────┘
```

Note: Model indicator moves to header in Cartwheel mode (phase-specific models).

---

## 3. Navigation Commands

| Mode | Command | Action |
|------|---------|--------|
| **Any** | `/help` | Show available commands for current mode |
| **Any** | `/chat` | Return to Chat mode |
| **Chat** | `/torch` | Show torch picker / create new |
| **Chat** | `/torches` | List all torches |
| **Torch** | `/torch` | Show current torch status |
| **Torch** | `/cartwheel` | Show cartwheel picker / create new |
| **Torch** | `/cartwheels` | List cartwheels in current torch |
| **Torch** | `/settings` | Torch settings |
| **Cartwheel** | `/cartwheel` | Show current cartwheel status |
| **Cartwheel** | `/phase` | Show current phase / transition options |
| **Cartwheel** | `/back` | Return to Torch mode |
| **Cartwheel** | `/requirements` | View/edit requirements (DnA artifact) |
| **Cartwheel** | `/decisions` | View/edit architectural decisions (AnP artifact) |
| **Cartwheel** | `/artifacts` | List all phase artifacts |

### 3.1 Navigation Flow Example

```
[Chat mode]
> /torches
  1. auth-system (3 cartwheels, AnP)
  2. billing-api (1 cartwheel, TnI)

> 1
[Enters Torch mode: auth-system]

> /cartwheels
  1. user-registration (AnP) ← active
  2. rbac-permissions (DnA)
  3. session-management (not started)

> 2
[Enters Cartwheel mode: rbac-permissions, DnA phase]

> /back
[Returns to Torch mode: auth-system]

> /chat
[Returns to Chat mode]
```

---

## 4. Phase Transitions: INCEPTION vs ITERATION

### 4.1 Two Workflow Modes

```
┌─────────────────────────────────────────────────────────────────┐
│                    INCEPTION SPRINT                              │
│           (Rigid, Gated, Full DnA→AnP→TnI→DnO cycle)            │
│                                                                  │
│   Trigger: New torch creation                                    │
│   Goal: Establish foundation + Walking Skeleton                  │
│   Output: One working vertical slice, all infra in place         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                   ITERATION SPRINTS                              │
│          (Flexible, Iterative, Per-Cartwheel cycles)            │
│                                                                  │
│   Trigger: Post-inception work                                   │
│   Goal: Build out remaining cartwheels from Context Map         │
│   Output: Incremental features, continuous delivery              │
└─────────────────────────────────────────────────────────────────┘
```

### 4.2 INCEPTION Sprint Phases

#### INCEPTION DnA

| Step | Activity | Output |
|------|----------|--------|
| 1a | Create VISION document (200-500 words) | `VISION.md` |
| 1b | Hecate conversation to refine vision | Agreement on vision |
| 1c | Event Storming workshop | `EVENT_STORMING.md`, Events/Facts |
| 1d | Context Map creation | `CONTEXT_MAP.md`, `context_map.yaml` |

**Event Storming UX (Hybrid ASCII):**

```
Hecate: Let's discover events. What happens when a user arrives?

You: They sign up with email or OAuth

Hecate: I've captured:

  ┌──────────────────┐    ┌──────────────────┐
  │ 🟧 UserSignedUp  │    │ 🟧 OAuthLinked   │
  │   via Email      │    │   via Provider   │
  └──────────────────┘    └──────────────────┘

  What happens next? Or type /board to see full board.

You: /board

  ══════════════ EVENT STORMING: auth-system ══════════════

  Timeline ──────────────────────────────────────────────►

  ┌────────────┐  ┌────────────┐  ┌────────────┐
  │🟧 UserSign │  │🟧 OAuthLin │  │🟧 UserVeri │
  │  edUp      │  │  ked       │  │  fied      │
  └────────────┘  └────────────┘  └────────────┘

  Legend: 🟧 Event  🟦 Command  🟩 Aggregate  🟨 External
  ════════════════════════════════════════════════════════
```

#### INCEPTION AnP

| Step | Activity | Output |
|------|----------|--------|
| 2a | Select first Cartwheel from Context Map | User picks bounded context |
| 2b | Scaffold CMD service | `initiate_{aggregate}` spoke |
| 2c | Scaffold QRY+PRJ service | `query_{aggregate}_by_id` spoke |
| 2d | Scaffold TUI (Go + Bubble Tea) | Separate repo |
| 2e | (Optional) Scaffold Web UI (Phoenix LiveView) | Separate repo |
| 2f | Create Kanban boards | DnA/AnP/TnI/DnO boards |

#### INCEPTION TnI

| Step | Activity | Output |
|------|----------|--------|
| 3a | Create Git repos | All repos initialized |
| 3b | Implement + test CMD spoke | Passing unit + integration tests |
| 3c | Implement + test QRY+PRJ spoke | Passing projection tests |
| 3d | Implement + test TUI | Working commands |
| 3e | (Optional) Implement + test Web UI | Working LiveView |

#### INCEPTION DnO

| Step | Activity | Output |
|------|----------|--------|
| 4a | CI/CD pipeline | Automated build + deploy |
| 4b | (TBD) Monitoring dashboard | Observability |
| 4c | (TBD) Ticketing system | Issue tracking |

### 4.3 Post-INCEPTION Flexibility

After INCEPTION completes:

- **Skip phases**: Small features can go straight to TnI
- **Parallel cartwheels**: Work on multiple cartwheels simultaneously
- **Soft suggestions**: Hecate suggests phase transitions, no hard gates

```
Hecate: You've been exploring requirements for a while.
        Consider `/phase anp` when ready to start designing.
```

---

## 5. Tools per Phase

### 5.1 Tool Categories

| Category | Examples | Invoked By |
|----------|----------|------------|
| **LLM Tools** | Read, Edit, Write, Grep, WebSearch | Hecate directly |
| **User Tools** | nvim, yazi, fzf, lazygit | Hecate launches, user interacts |
| **Hybrid Tools** | Terminal (Bash), Browser | Either |

### 5.2 Phase-Specific Tool Access

| Phase | LLM Tools | User Tools | Blocked |
|-------|-----------|------------|---------|
| **DnA** | Read, Grep, WebSearch, WebFetch | nvim (docs), yazi, browser | Edit, Write, Bash (mutating) |
| **AnP** | Above + Write (scaffolds only) | nvim (design docs), fzf | Bash (builds) |
| **TnI** | All tools | nvim, lazygit, fzf, test runners | - |
| **DnO** | All + Deploy, CI/CD | lazygit, k9s, monitoring | - |

### 5.3 Phase-Specific LLM Behavior

```
# DnA System Prompt Addition
"You are in Discovery & Analysis phase. Your role is to:
- Ask clarifying questions
- Document requirements
- Facilitate event storming
- DO NOT write implementation code
- DO NOT suggest moving to implementation yet"

# TnI System Prompt Addition
"You are in Testing & Implementation phase. Your role is to:
- Write production code
- Write tests first (TDD encouraged)
- Run builds and tests
- Fix issues iteratively"
```

### 5.4 User Tool Configuration

```toml
# ~/.config/hecate-tui/config.toml

[tools]
editor = "nvim"
file_browser = "yazi"
fuzzy_finder = "fzf"
git_client = "lazygit"
terminal = "kitty"
browser = "firefox"

[tools.shortcuts]
edit = "e"
browse = "b"
git = "g"
find = "f"
```

### 5.5 Tool Launch UX

```
Hecate: I've created the initial spoke structure:

  apps/manage_users/src/register_user/
  ├── register_user_v1.erl
  ├── user_registered_v1.erl
  └── maybe_register_user.erl

  [e] Open in nvim    [b] Browse in yazi    [g] View in lazygit

You: e

# TUI suspends, nvim opens
# On exit, TUI resumes
```

---

## 6. Git Change Detection

### 6.1 Detection Mechanism

```
1. Before tool launch:
   snapshot = git_status() + git_log(-1)

2. Suspend TUI, launch tool

3. On tool exit / TUI resume:
   current = git_status() + git_log(-1)
   diff = compare(snapshot, current)

4. If diff detected:
   - New commits? Show summary
   - Modified files? Offer review
   - New files? Offer to discuss
```

### 6.2 Change Detection UX

```
[User returns from nvim]

Hecate: Welcome back. I noticed you made changes:

  Modified (unstaged):
    apps/manage_users/src/register_user/maybe_register_user.erl

  +14 -3 lines changed

  [r] Review changes    [t] Run tests    [c] Continue chatting
```

### 6.3 External Change Detection

```
[User ran `git pull` in another terminal]

Hecate: I noticed the repository was updated externally:

  3 new commits from origin/main:
    - feat: add password reset flow (alice)
    - fix: session timeout bug (bob)
    - docs: update API reference (alice)

  Should I summarize these changes?
```

### 6.4 Configuration

```toml
[tools.detection]
git_poll_interval = 5      # seconds, 0 to disable
watch_files = true         # inotify-based file watching
auto_summarize = false     # auto-summarize or ask first
```

---

## 7. Repository Structure

### 7.1 Multi-Repo with Meta-Repo

```
github.com/your-org/
├── auth-system/                    # META-REPO (the Torch)
│   ├── VISION.md
│   ├── CONTEXT_MAP.md
│   ├── context_map.yaml
│   ├── repos.yaml
│   ├── inception/
│   │   └── EVENT_STORMING.md
│   └── .hecate/
│       └── torch.json
│
├── auth-daemon/                    # CMD + QRY (Erlang umbrella)
│   └── apps/
│       ├── manage_users/
│       ├── query_users/
│       └── ...
│
├── auth-tui/                       # TUI (Go + Bubble Tea)
│
└── auth-web/                       # Web UI (Phoenix LiveView)
```

### 7.2 repos.yaml

```yaml
components:
  daemon:
    repo: auth-daemon
    type: erlang-umbrella
    contains: [cmd, qry]
  tui:
    repo: auth-tui
    type: go-bubbletea
  web:
    repo: auth-web
    type: phoenix-liveview
    optional: true
```

### 7.3 context_map.yaml

```yaml
torch: auth-system
bounded_contexts:
  - name: user-registration
    type: cmd
    aggregates: [user]
    events: [UserSignedUp, UserVerified, OAuthLinked]
    status: inception

  - name: session-management
    type: cmd
    aggregates: [session]
    events: [SessionStarted, SessionExpired]
    status: pending
```

---

## 8. Implementation Phases

### Phase 1: Modal Infrastructure
- [ ] Mode state machine (Chat → Torch → Cartwheel)
- [ ] Header bar component (appears/disappears based on mode)
- [ ] Navigation commands (`/torch`, `/torches`, `/cartwheel`, `/cartwheels`, `/back`, `/chat`)
- [ ] Context detection on startup (git remote, `.hecate/torch.json`)

### Phase 2: Torch Management
- [ ] Torch CRUD via daemon API
- [ ] Torch picker UI
- [ ] Torch status display
- [ ] Meta-repo initialization

### Phase 3: Cartwheel & Phase Management
- [ ] Cartwheel CRUD via daemon API
- [ ] Cartwheel picker UI
- [ ] Phase display in header
- [ ] Phase transition commands and suggestions

### Phase 4: INCEPTION Workflow
- [ ] Vision document creation flow
- [ ] Event Storming facilitation (ASCII board)
- [ ] Context Map generation
- [ ] Scaffolding automation (CMD, QRY, TUI, Web)

### Phase 5: Tool Integration
- [ ] User tool configuration
- [ ] Tool launch with TUI suspend/resume
- [ ] Git change detection on resume
- [ ] External change polling

### Phase 6: Phase-Specific Behavior
- [ ] Per-phase system prompts
- [ ] Per-phase tool restrictions
- [ ] Per-phase model selection (future: fine-tuned models)

---

## 9. Open Questions

1. **Kanban board implementation** - External tool (GitHub Projects, Trello) or built-in?
2. **Multi-user collaboration** - How do multiple team members work on same torch?
3. **Offline support** - What happens when daemon is unreachable?
4. **Phase artifact validation** - How strict should gating be in INCEPTION?

---

## 10. References

- hecate-agents/philosophy/DDD.md
- hecate-agents/philosophy/CARTWHEEL.md
- hecate-agents/philosophy/VERTICAL_SLICING.md
- hecate-agents/skills/ANTIPATTERNS.md
