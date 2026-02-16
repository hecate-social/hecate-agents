# ALC: Deployment -- Ship to Environments

*Process 6 of [HECATE_ALC](HECATE_ALC.md)*

[Back to ALC Index](HECATE_ALC.md)

---

## Purpose

Get the division's software from a tested artifact into running environments. Deployment is not "push and pray" -- it is a deliberate, repeatable, auditable process where every version is traceable, every change flows through GitOps, and every rollback is a forward action.

**Deployment is where discipline meets production.**

---

## Mindset

```
"I deploy small and often.
 I never build locally for production.
 I tag everything.
 Every deployment is traceable to a commit."
```

---

## Lifecycle

| Command | Event | Transition |
|---------|-------|------------|
| `open_deployment` | `deployment_opened_v1` | pending --> active |
| `shelve_deployment` | `deployment_shelved_v1` | active --> paused |
| `resume_deployment` | `deployment_resumed_v1` | paused --> active |
| `conclude_deployment` | `deployment_concluded_v1` | active --> completed |

A deployment is opened when a debugged, tested artifact is ready to ship. It is shelved when blocked (e.g., environment unavailable, dependency not ready). It is concluded when the artifact is running in the target environment and smoke tests pass.

---

## Activities

### 1. Release Preparation

Before deploying, the artifact must be tagged and documented.

**Release checklist:**

- [ ] Version number assigned (semver: major.minor.patch)
- [ ] CHANGELOG updated with version entry
- [ ] Release notes written
- [ ] Breaking changes documented
- [ ] Migration steps documented (if any)
- [ ] Rollback plan ready

**Release template:**

```markdown
## Release v{X.Y.Z}

### Changes
- feat: ...
- fix: ...

### Breaking Changes
- None / List them

### Migration Steps
- None / List them

### Rollback Plan
- Revert to v{previous}
- Data migration rollback: ...
```

---

### 2. The GitOps Flow

**The Golden Rule: Code Repo and GitOps Repo are separate.**

```
+----------------------------------------------------------------+
|  CODE REPO (e.g., hecate-daemon)                               |
|  - Source code, tests, Dockerfile                              |
|  - Semantic versioning in app.src / mix.exs                    |
|  - Git tags for releases (v0.7.3)                              |
|  - CI builds docker images on tag push                         |
+----------------------------------------------------------------+
                              |
                              v
+----------------------------------------------------------------+
|  CONTAINER REGISTRY (ghcr.io)                                  |
|  - Images tagged with version (ghcr.io/org/app:v0.7.3)        |
|  - Immutable once pushed                                       |
+----------------------------------------------------------------+
                              |
                              v
+----------------------------------------------------------------+
|  GITOPS REPO (e.g., hecate-gitops)                             |
|  - Kubernetes manifests only                                   |
|  - References specific image tags                              |
|  - Flux / ArgoCD watches this repo                             |
+----------------------------------------------------------------+
                              |
                              v
+----------------------------------------------------------------+
|  CLUSTER                                                       |
|  - Flux reconciles GitOps repo to actual state                 |
|  - Pulls images from registry                                  |
+----------------------------------------------------------------+
```

---

### 3. Complete Deployment Flow (Example)

```bash
# STEP 1: CODE REPO -- Bump version
cd ~/work/github.com/hecate-social/hecate-daemon
# Edit src/hecate.app.src: {vsn, "0.7.3"}

# STEP 2: CODE REPO -- Commit, tag, push
git add -A && git commit -m "chore: Bump version to 0.7.3"
git tag v0.7.3
git push origin main
git push origin v0.7.3

# STEP 3: CI BUILDS AUTOMATICALLY
# GitHub Actions triggers on tag push:
# - .github/workflows/docker.yml builds multi-arch image
# - Pushes to ghcr.io/hecate-social/hecate-daemon:0.7.3
# Monitor: gh run list --repo hecate-social/hecate-daemon
# NEVER build docker images locally for production!

# STEP 4: GITOPS REPO -- Update image tag (after CI completes)
cd ~/work/github.com/hecate-social/hecate-gitops
# Edit infrastructure/hecate/daemonset.yaml:
#   image: ghcr.io/hecate-social/hecate-daemon:v0.7.3
git add -A && git commit -m "chore: Bump hecate-daemon to v0.7.3"
git push origin main

# STEP 5: CLUSTER -- Pull and reconcile (on control plane node)
ssh beam00
cd ~/.hecate/gitops && git pull
flux reconcile kustomization hecate-infrastructure
# Or wait for Flux to auto-reconcile (1 minute interval)
```

**Rollback is a forward action:**

```bash
# To "rollback" to v0.7.2:
cd ~/work/github.com/hecate-social/hecate-gitops
# Edit daemonset.yaml: image: ghcr.io/.../hecate-daemon:v0.7.2
git commit -m "fix: Rollback to v0.7.2 due to {reason}"
git push origin main
# Flux deploys v0.7.2
```

Rollback is just deploying an older known-good version -- via the same GitOps flow. There is no separate rollback mechanism. There is no `kubectl set image`. There is no SSH-and-restart.

---

### 4. Deployment Strategies

| Strategy | Use When | Risk | How |
|----------|----------|------|-----|
| **Rolling** | Standard deploys | Low | Kubernetes default -- old pods replaced one at a time |
| **Blue/Green** | Zero-downtime critical | Medium | Two full environments, switch traffic |
| **Canary** | High-risk changes | Low (gradual) | Route small % of traffic to new version |
| **Recreate** | Breaking changes requiring clean state | High (downtime) | Kill all old pods, start new ones |

For most Hecate divisions, **rolling** is the default. Use canary for changes that touch the mesh or event store schema. Use recreate only when old and new versions cannot coexist.

---

### 5. Smoke Testing

**Immediately after deployment, verify the artifact is alive and functional.**

```bash
# Health check
curl --unix-socket /run/hecate/daemon.sock http://localhost/health
# Expected: {"status": "ok", "version": "0.7.3"}

# Basic API functionality
curl --unix-socket /run/hecate/daemon.sock http://localhost/api/{resource}
# Expected: 200 OK

# Critical path test
# Whatever is most important to the division -- verify it works.
```

Automated smoke tests should run on every deployment. If a smoke test fails, the deployment is not concluded -- it is shelved or the version is rolled back.

---

## Anti-Patterns

| Anti-Pattern | Problem | Instead |
|--------------|---------|---------|
| `image: app:latest` | No traceability, unpredictable pulls | Explicit version tags: `app:v0.7.3` |
| `image: app:main` | Same tag, different content over time | Immutable version tags |
| `imagePullPolicy: Always` | Hides what version is actually running | Use specific tags, pull only on change |
| `kubectl set image ...` | Bypasses GitOps, causes state drift | Update GitOps manifests |
| Restarting pods manually | Masks the real issue, no audit trail | Version bump, tag, CI, GitOps |
| Skipping the version bump | Cannot tell what is deployed | Always bump, always tag |
| Building images locally | Unreproducible, no audit trail | Let CI build from the git tag |
| Deploy and forget | Issues go unnoticed until users complain | Hand off to [Monitoring](HECATE_ALC_MONITORING.md) |
| No rollback plan | Stuck when things break | Always document the rollback path |
| Manual deployments | Inconsistent, error-prone | GitOps always |

---

## Entry Checklist (Before Opening Deployment)

- [ ] Debugging concluded -- tests pass, quality verified
- [ ] Version number assigned (semver)
- [ ] CHANGELOG updated
- [ ] Release notes written
- [ ] Breaking changes documented
- [ ] Rollback plan documented
- [ ] CI/CD pipeline green
- [ ] STAGING verified (if applicable)
- [ ] Team notified

---

## Exit Checklist (Before Concluding Deployment)

- [ ] Artifact deployed to target environment
- [ ] Health checks passing
- [ ] Smoke tests passing
- [ ] No error spikes in initial observation window
- [ ] Version traceable in cluster (correct image tag running)
- [ ] Monitoring process ready to open (hand off to [Monitoring](HECATE_ALC_MONITORING.md))
- [ ] Users unaffected or notified of changes

---

## Transition

**Inbound:** From [Debugging](HECATE_ALC_DEBUGGING.md) (process 5). Debugging concluded, artifact is tested and ready.

**Outbound:** To [Monitoring](HECATE_ALC_MONITORING.md) (process 7). Deployment concluded, artifact is running -- now observe it.

**Escalation:** If deployment reveals issues that cannot be fixed by rollback, open [Rescue](HECATE_ALC_RESCUE.md).

---

*Tag it. Ship it. Verify it. Move on.*
