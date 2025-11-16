# Realm Engine

**Active Documentation Through Game DSL**

A Haskell adventure game engine demonstrating executable documentation, type-safe parsing, and feedback loops through interactive gameplay.

Based on [adventure-engine](https://github.com/agentultra/adventure-engine) by James King.

## What This Demonstrates

**Episode 6: The Haskell DSL Adventure**

Shows how a game DSL creates a tight feedback loop:
- Megaparsec parser (text → typed data)
- Type-safe game engine
- Orchestrated demonstrations
- Active documentation principles

## Quick Start

```bash
# Build
stack build

# Play example world
stack exec adventure-cli data/example-adventure/world.txt

# Run Episode 6 demo
./tools/e06-master-script.sh
```

## The DSL Stack

```
world.txt (declarative game content)
    ↓
Megaparsec Parser (Maker.Parser.hs)
    ↓
Typed Haskell Data (Adventure.Engine)
    ↓
Game Commands (pickup, walk, use, dig)
    ↓
Feedback Loop (player interaction)
```

## Demo Tools

- **e06-master-script.sh** - Complete orchestrated demo
- **type-to-pane.sh** - Natural typing simulator
- **term-box.sh** - Terminal-aware formatting
- **show-card.sh** - Card display system

## Attribution

See [ATTRIBUTION.md](ATTRIBUTION.md) for complete credits.

**Core engine:** James King (agentultra/adventure-engine)  
**License:** BSD-3-Clause
