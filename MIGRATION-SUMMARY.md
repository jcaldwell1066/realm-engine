# Realm Engine - Migration Summary

**Date:** 2025-11-15
**From:** ~/projects/experiments/skunkworks/exploration/adventure-engine
**To:** ~/projects/realm-engine
**Published:** https://github.com/jcaldwell1066/realm-engine

---

## What Happened

### Initial Publication
Created clean public repo from exploration prototype:
- Extracted core game engine (Haskell + Megaparsec)
- Added E06 demo system (cards, scripts, tools)
- Preserved attribution to agentultra/adventure-engine (BSD-3-Clause)
- Published with clean git history

### Merge Strategy

**Branches found:**
1. `master` - Our clean baseline (2 commits)
2. `claude/project-checkout-...` - Build verification + missing executables
3. `claude/check-wsl-packages-...` - Docker + WSL packages

**Actions taken:**
1. Merged `project-checkout` → `master` (added world-loader, adventure-tui)
2. Resolved conflict in test/Spec.hs
3. Added .claude/CLAUDE-CODE-WORKFLOW.md for consistency

**Result:** master now has complete build + all executables

---

## Current State

**Published repo includes:**
- ✅ Haskell source (Megaparsec parser, game engine)
- ✅ 4 executables (adventure-cli, adventure-maker, adventure-tui, world-loader)
- ✅ E06 demo system (14 cards, 3 scripts, orchestrator)
- ✅ Tools (type-to-pane.sh, term-box.sh, show-card.sh)
- ✅ Tests + .gitignore
- ✅ INSTALL.md (Debian packages documented)
- ✅ LICENSE + ATTRIBUTION (upstream credited)
- ✅ .claude/CLAUDE-CODE-WORKFLOW.md (maintenance guide)

---

## Merge Strategy Decision

### Why merge project-checkout?
- ✅ Had world-loader/Main.hs (needed for PostgreSQL loading)
- ✅ Had adventure-tui/Main.hs (TUI interface)
- ✅ Build was verified working
- ✅ RealmDSL.hs fix (System.Process import commented out)

### Why not merge WSL branch (yet)?
- ⚠️ Docker files are useful but not critical
- ⚠️ Can cherry-pick later if needed
- ⚠️ Keeps master clean for initial release

---

## Branch Workflow Going Forward

```
master (stable, public-facing)
   ↑
   │ PR after testing
   │
dev (integration branch) - CREATE THIS
   ↑
   │ PR for review
   │
feature/* (experiments)
```

**Recommendation:**
1. Create `dev` branch from master
2. Do active work on `dev` or feature branches
3. PR to master only when stable + tested
4. Let Claude Code auto-create feature branches

---

## For Other Claude Code Instances

**Read first:** `.claude/CLAUDE-CODE-WORKFLOW.md`

**Key points:**
- Always start my-context session
- Build before committing: `stack build && stack test`
- No PII, payment data, or credentials
- Use feature branches for experiments
- Clear commit messages
- Attribution must be preserved

**Starting work:**
```bash
cd ~/projects/realm-engine
my-context start "feature-name-2025-MM-DD"
git checkout -b feature/my-feature
# ... do work ...
stack build && stack test
git commit -m "Clear message"
gh pr create --base dev
my-context export feature-name > docs/sessions/feature-name.md
```

---

## What to Work On Next

**Potential features:**
1. Improve E06 demo coordination (ongoing)
2. Add more example worlds (clean, no PII)
3. Enhance natural language mapping
4. Add more tests
5. CI/CD workflow (.github/workflows/)
6. Better error messages from parser
7. Documentation improvements

**Don't do:**
- Add payment-related examples
- Include company-specific content
- Commit without testing
- Force-push to master

---

## Migration Complete

**Old location** (archive, don't work here):
```
~/projects/experiments/skunkworks/exploration/adventure-engine
```

**New location** (active work):
```
~/projects/realm-engine
```

**Published:**
```
https://github.com/jcaldwell1066/realm-engine
```

All Claude Code sessions should work from `~/projects/realm-engine` now.

---

## Context Sessions Used

- `dsl-stack-clarity-2025-11-15` (1h 24m) - DSL clarification, E06 development
- `github-repo-planning-2025-11-15` (13m) - Repo planning and publication
- `pii-check-2025-11-15` (3m) - PII verification before publish
- `repo-migration-2025-11-15` (3m) - Top-level migration
- `git-strategy-2025-11-15` (11h 10m) - Merge strategy

All tracked and exportable.

---

*Migration complete - ready for public collaboration!* 🚀
