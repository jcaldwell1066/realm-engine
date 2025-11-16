# boxes-live Integration - Quick Start

## What's New

New integration between **realm-engine** and **boxes-live** for interactive world map visualization!

## New Components

### 1. BoxesLive Module
**File:** `src/Adventure/BoxesLive.hs`

Converts realm-engine `World` to boxes-live canvas format:
```haskell
import Adventure.BoxesLive

-- Convert world to canvas
let canvas = worldToCanvas myWorld

-- Export to JSON
let json = exportCanvasJSON canvas
```

### 2. Exporter Executable
**File:** `realm-boxes-exporter/Main.hs`

Command-line tool to export worlds from PostgreSQL to JSON:
```bash
stack exec realm-boxes-exporter -- \
  "host=localhost dbname=adventure user=postgres" \
  world.json
```

### 3. Tests
**File:** `test/Adventure/BoxesLiveSpec.hs`

Unit tests for the integration:
```bash
stack test --test-arguments "-m BoxesLive"
```

## Quick Start

### Step 1: Build
```bash
stack build
```

### Step 2: Create a World
```bash
# Use adventure-maker to load a world
stack exec adventure-maker -- data/example-adventure/world.txt
```

### Step 3: Export to JSON
```bash
stack exec realm-boxes-exporter -- \
  "host=localhost dbname=adventure user=postgres" \
  world.json
```

### Step 4: Inspect the Output
```bash
jq . world.json
```

Output format:
```json
{
  "boxes": [
    {
      "id": "room_1",
      "position": {"x": 0, "y": 0},
      "width": 40,
      "height": 10,
      "content": {
        "title": "Room Name",
        "lines": ["Description", "Items:", "  • item1"],
        "tags": ["room", "items"]
      },
      "isPlayerLocation": true
    }
  ],
  "connections": [
    {"from": "room_1", "to": "room_2", "label": "north"}
  ],
  "metadata": {
    "type": "realm-engine-world",
    "playerRoom": "room_1"
  }
}
```

## Next Steps

1. **Enhance boxes-live** to import this JSON format
2. **Visualize** the world interactively
3. **Navigate** between rooms with keyboard
4. **Real-time updates** via IPC (future phase)

## Documentation

- **Full integration guide:** `docs/BOXES-LIVE-INTEGRATION.md`
- **Architecture:** Phase 1 (JSON export) complete
- **Roadmap:** Phases 2-4 planned

## Files Modified

```
realm-engine/
├── src/Adventure/BoxesLive.hs         # NEW: Integration module
├── realm-boxes-exporter/Main.hs       # NEW: Export utility
├── test/Adventure/BoxesLiveSpec.hs    # NEW: Tests
├── test/Spec.hs                       # UPDATED: hspec-discover
├── package.yaml                       # UPDATED: New executable + deps
└── docs/
    ├── BOXES-LIVE-INTEGRATION.md      # NEW: Full architecture
    └── INTEGRATION-QUICKSTART.md      # NEW: This file
```

## Testing

```bash
# Run all tests
stack test

# Run only BoxesLive tests
stack test --test-arguments "-m BoxesLive"

# Build and test
stack build && stack test
```

## Troubleshooting

### Build fails
```bash
stack clean
stack build
```

### Missing database
Make sure PostgreSQL is running and the database is loaded:
```bash
sudo systemctl status postgresql
stack exec adventure-maker -- data/example-adventure/world.txt
```

### Export fails
Check database connection string:
```bash
psql -h localhost -U postgres -d adventure -c "SELECT version();"
```

## Integration with boxes-live

Once boxes-live supports JSON import:
```bash
# Export from realm-engine
stack exec realm-boxes-exporter -- "..." world.json

# Visualize in boxes-live
boxes-live --import world.json
```

**See:** https://github.com/jcaldwell1066/boxes-live

---

**Happy mapping! 🗺️**
