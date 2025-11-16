# Claude Code Workflow for realm-engine

## Repository Overview

**Public repo:** https://github.com/jcaldwell1066/realm-engine

**Purpose:** Haskell adventure game engine demonstrating active documentation through executable DSLs

**License:** BSD-3-Clause (preserves upstream attribution to agentultra/adventure-engine)

---

## Branch Strategy

### master (default branch)
- **Stable, tested code only**
- All commits should build successfully
- Run `stack test` before pushing
- This is what users will clone

### dev (ongoing work)
- Active development branch
- Feature integration happens here
- Can be unstable
- Merge to master when stable

### Feature branches
- Name: `feature/descriptive-name` or let Claude Code auto-name
- One feature per branch
- PR to dev, then dev → master
- Delete after merge

---

## Commit Guidelines

### Good commit messages:
```
Add world-loader executable for database loading

- Implements PostgreSQL world loading
- Supports entity ID resolution
- Includes error handling for missing entities
```

### Bad commit messages:
```
Fix stuff
Updates
WIP
```

### Format:
```
<type>: <subject>

<body explaining why, not what>

<footer: references, breaking changes>
```

Types: Add, Fix, Update, Refactor, Docs, Test

---

## Before Committing

**Always run:**
```bash
stack build    # Must succeed
stack test     # Tests must pass
```

**Check for:**
- No PII or payment data
- No API credentials
- No company-specific content
- Attribution preserved in LICENSE/ATTRIBUTION.md

---

## Working with Claude Code

### Starting a session

1. Always start my-context:
```bash
export MY_CONTEXT_HOME=db
my-context start "feature-name-2025-MM-DD"
```

2. Log your intent:
```bash
my-context note "Adding X feature to demonstrate Y"
```

3. Track files as you work:
```bash
my-context file src/NewModule.hs
```

### During development

- Keep commits focused (one feature/fix per commit)
- Build frequently: `stack build`
- Test frequently: `stack test`
- Log decisions: `my-context note "Chose X because Y"`

### Finishing up

1. Export context:
```bash
my-context export session-name > docs/sessions/session-name.md
```

2. Create PR (if on feature branch):
```bash
gh pr create --base dev --title "Add X feature" --body "$(my-context export session-name)"
```

3. Stop context:
```bash
my-context stop
```

---

## Project Structure

```
realm-engine/
├── src/                    # Haskell source (game engine)
│   ├── Adventure/
│   │   └── Engine/        # Core engine
│   └── Maker/             # World parser (Megaparsec)
├── app/                   # Main executable
├── adventure-cli/         # CLI player
├── adventure-maker/       # World compiler
├── adventure-tui/         # TUI interface
├── world-loader/          # PostgreSQL loader
├── test/                  # Unit tests
├── tools/                 # Demo/orchestration scripts
├── demos/                 # Episode demos (E06, etc.)
├── data/                  # Example worlds
└── docs/                  # Documentation
```

---

## Key Files

**Don't modify without discussion:**
- LICENSE (BSD-3-Clause, upstream attribution)
- ATTRIBUTION.md (credit to original authors)
- package.yaml (build configuration)

**Safe to modify:**
- README.md (keep it current)
- INSTALL.md (add helpful tips)
- docs/* (documentation)
- demos/* (demo content)

**Auto-generated (don't commit):**
- adventure-engine.cabal (from package.yaml)
- stack.yaml.lock (from stack build)
- .stack-work/ (build artifacts)

---

## Testing

### Run all tests:
```bash
stack test
```

### Run specific test:
```bash
stack test --test-arguments "-m DatabaseLoader"
```

### Add new tests:
Place in `test/` directory, naming: `*Spec.hs`

---

## Building

### Full build:
```bash
stack build
```

### Fast iteration:
```bash
stack build --fast --file-watch
```

### Clean build:
```bash
stack clean
stack build
```

---

## Demos

### Episode 6 Demo

Run the orchestrated demo:
```bash
./tools/e06-master-script.sh
```

This demonstrates:
- Megaparsec parser
- Type-safe game engine
- Feedback loops
- Natural typing simulation

### Manual play:
```bash
stack exec adventure-cli data/example-adventure/world.txt
```

---

## Adding New Features

### Example: Adding new command

1. Start context:
```bash
my-context start "add-combine-command-2025-MM-DD"
```

2. Update parser (src/Maker/Parser.hs):
```haskell
validVerbs = [...existing..., "combine"]
```

3. Add engine logic (src/Adventure/Engine.hs or RealmDSL.hs)

4. Test it:
```bash
stack build && stack test
```

5. Document:
```bash
my-context note "Added combine command for item crafting"
my-context file src/Maker/Parser.hs
```

6. Commit:
```bash
git add src/Maker/Parser.hs src/RealmDSL.hs
git commit -m "Add combine command for item crafting

Allows players to combine items to create new objects.
Parser recognizes 'combine X with Y' syntax.
Engine validates items exist and are combinable.
"
```

---

## Demo Tool Usage

### Natural typing simulator:
```bash
./tools/type-to-pane.sh <pane> "command to type"
./tools/type-to-pane.sh <pane> --script demo.txt
```

### Terminal-aware boxes:
```bash
echo "text" | ./tools/term-box.sh --center --fill
```

### Show presentation card:
```bash
./tools/show-card.sh <pane> e06-cards/01-title.txt 3
```

---

## Publishing Changes

### To master (stable):
```bash
# Make sure you're on master and up to date
git checkout master
git pull

# Your changes should be tested
stack build && stack test

# Commit with good message
git add <files>
git commit -m "Clear, descriptive message"

# Push
git push
```

### To feature branch:
```bash
# Create branch
git checkout -b feature/my-feature

# Make changes, commit
git add <files>
git commit -m "Add my feature"

# Push
git push -u origin feature/my-feature

# Create PR
gh pr create --base dev --title "Add my feature"
```

---

## Branch Workflow

```
master (stable, public-facing)
   ↑
   │ (PR after testing)
   │
dev (integration, can be unstable)
   ↑
   │ (PR for review)
   │
feature/my-feature (experiments)
```

---

## What NOT to Commit

❌ API keys or credentials
❌ Payment-related data
❌ PII (personal information)
❌ Company-specific content
❌ .stack-work/ directory
❌ Build artifacts (.o, .hi files)
❌ Personal my-context exports (unless anonymized)

✅ Generic example worlds
✅ Documentation
✅ Tests
✅ Demo scripts

---

## Current State (2025-11-15)

**Published:**
- Initial release with game engine
- E06 demo system
- Build instructions

**Branches to handle:**
- claude/check-wsl-packages-... (Docker files - useful)
- claude/project-checkout-... (world-loader - merged)

**Next up:**
- Establish dev branch
- Clean up feature branches
- Add CONTRIBUTING.md

---

## Quick Reference

```bash
# Start work
my-context start "task-2025-MM-DD"
git checkout -b feature/task

# During work
stack build --file-watch
my-context note "decisions"

# Finish
stack test
git commit -m "Clear message"
gh pr create --base dev
my-context export task > docs/sessions/task.md
my-context stop
```

---

## Getting Help

- README.md - Quick start
- INSTALL.md - Setup instructions
- docs/GAME-DSL-STACK-PROPER.md - Architecture
- This file - Workflow
- GitHub Issues - Questions

---

*Keep the DSL alive! 🎮*
