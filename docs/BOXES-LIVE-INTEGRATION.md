# boxes-live Integration Architecture

## Overview

**realm-engine** (Haskell adventure game engine) + **boxes-live** (C terminal canvas) = Interactive world map visualization

This document describes the integration architecture for visualizing realm-engine worlds using boxes-live as an interactive terminal canvas.

## Projects

### realm-engine
- **Language:** Haskell
- **Purpose:** Adventure game engine with DSL-based world definition
- **Repository:** https://github.com/jcaldwell1066/realm-engine
- **Key features:**
  - Megaparsec-based world parser
  - PostgreSQL persistence
  - Multiple frontends (CLI, TUI, GUI)
  - JSON serialization of game state

### boxes-live
- **Language:** C (ncurses)
- **Purpose:** Terminal-based interactive canvas (like Miro for terminals)
- **Repository:** https://github.com/jcaldwell1066/boxes-live
- **Key features:**
  - Pan and zoom navigation
  - Box rendering with titles and content
  - Keyboard-driven interaction
  - Viewport/camera system

## Integration Approaches

### Approach 1: JSON Export + Import (Phase 1 - IMPLEMENTED)

**Status:** ✅ Complete - Ready for testing

**Flow:**
```
realm-engine (Haskell) → JSON export → boxes-live (C) → visualization
```

**Implementation:**
1. **New module:** `src/Adventure/BoxesLive.hs`
   - Data types: `BoxCanvas`, `Box`, `Position`, `BoxContent`
   - Function: `worldToCanvas :: World -> BoxCanvas`
   - JSON serialization for all types

2. **New executable:** `realm-boxes-exporter`
   - Connects to PostgreSQL
   - Loads world state
   - Exports to boxes-live JSON format
   - Usage: `realm-boxes-exporter "host=localhost dbname=adventure" world.json`

3. **JSON Schema:**
```json
{
  "boxes": [
    {
      "id": "room_1",
      "position": {"x": 0, "y": 0},
      "width": 40,
      "height": 10,
      "content": {
        "title": "Entrance Hall",
        "lines": [
          "You stand in a grand entrance hall",
          "",
          "Items:",
          "  • ancient key",
          "  • dusty map"
        ],
        "tags": ["room", "items"]
      },
      "isPlayerLocation": true
    }
  ],
  "connections": [
    {
      "from": "room_1",
      "to": "room_2",
      "label": "north door"
    }
  ],
  "metadata": {
    "type": "realm-engine-world",
    "version": "0.1.0",
    "playerRoom": "room_1"
  }
}
```

**Advantages:**
- ✅ Simple, no language barrier
- ✅ Stateless (export once, visualize)
- ✅ Easy debugging (human-readable JSON)
- ✅ Works with existing boxes-live code

**Limitations:**
- ❌ Not real-time (manual re-export for updates)
- ❌ One-way communication (view only)

**Next steps:**
1. Build realm-engine: `stack build`
2. Test exporter: `stack exec realm-boxes-exporter -- "..." world.json`
3. Enhance boxes-live to import this JSON format
4. Add visualization features (player marker, connections)

---

### Approach 2: IPC with Named Pipes/Sockets (Phase 2 - FUTURE)

**Status:** 🔄 Planned for future

**Flow:**
```
realm-engine ←→ Unix socket ←→ boxes-live
```

**Implementation:**
- realm-engine exports world state on update
- boxes-live polls/subscribes to updates
- Real-time map updates as player moves

**Use case:** Live game visualization during play

---

### Approach 3: FFI Bridge (Phase 3 - ADVANCED)

**Status:** 💡 Exploration

**Flow:**
```
realm-engine (Haskell) → FFI → boxes-live (C library)
```

**Implementation:**
- Compile boxes-live as a shared library
- Create Haskell FFI bindings
- Embed boxes-live rendering in adventure-tui

**Challenges:**
- Complex FFI design (memory management, callbacks)
- Mixing ncurses (C) with vty/brick (Haskell)
- State synchronization

**Benefits:**
- ✅ Deeply integrated experience
- ✅ Full control from Haskell
- ✅ Single binary

---

## Current Implementation Details

### Module: Adventure.BoxesLive

Located at: `src/Adventure/BoxesLive.hs`

**Exports:**
```haskell
worldToCanvas :: World -> BoxCanvas
exportCanvasJSON :: BoxCanvas -> ByteString
calculateRoomPositions :: [EntityId Room] -> Map (EntityId Room) Position
```

**Key types:**
- `BoxCanvas` - Full canvas with boxes, connections, metadata
- `Box` - Individual room representation
- `Position` - X/Y coordinates in world space
- `BoxContent` - Title, description lines, tags

### Executable: realm-boxes-exporter

Located at: `realm-boxes-exporter/Main.hs`

**Usage:**
```bash
# Export to file
realm-boxes-exporter "host=localhost dbname=adventure user=postgres" world.json

# Export to stdout
realm-boxes-exporter "host=localhost dbname=adventure" - | jq .

# Help
realm-boxes-exporter --help
```

**Dependencies:**
- PostgreSQL connection (uses existing `Adventure.Engine.Database.Loader`)
- JSON encoding (via `aeson`)

---

## Room Layout Algorithm

**Current:** Grid-based layout
- Arrange rooms in a square grid
- Column count: `ceil(sqrt(num_rooms))`
- Spacing: 60 units (X), 20 units (Y)

**Future enhancements:**
- Force-directed graph layout
- Manual positioning hints from world DSL
- Respect explicit room coordinates
- Cluster by region/zone

---

## boxes-live Enhancements Needed

To fully support realm-engine visualization:

1. **JSON Import**
   - Add `--import <file>` flag
   - Parse JSON into canvas structure
   - Initialize boxes from imported data

2. **Visual Distinctions**
   - Highlight player location (different border style)
   - Show connections between rooms (arrows/lines)
   - Tag-based coloring (items, locked doors, etc.)

3. **Interactive Features**
   - Click/select room to see details
   - Navigate between connected rooms
   - Filter by tags (show only rooms with items)

4. **Export Updated Positions**
   - Allow manual repositioning in boxes-live
   - Export updated layout for persistence
   - Re-import layout hints in realm-engine

---

## Example Workflow

### Step 1: Create a world in realm-engine
```bash
# Use adventure-maker to create world from DSL
stack exec adventure-maker -- data/my-world.txt

# This loads into PostgreSQL
```

### Step 2: Export to boxes-live format
```bash
# Export world state
stack exec realm-boxes-exporter -- \
  "host=localhost dbname=adventure user=postgres" \
  world.json
```

### Step 3: Visualize in boxes-live
```bash
# Import and explore (once boxes-live supports import)
boxes-live --import world.json

# Navigate with arrow keys
# Zoom with +/-
# Pan around the world
```

### Step 4: Update and re-export
```bash
# Play the game
stack exec adventure-cli -- data/my-world.txt

# Re-export after changes
stack exec realm-boxes-exporter -- "..." world-updated.json

# Visualize updated state
boxes-live --import world-updated.json
```

---

## Integration Roadmap

### ✅ Phase 1: JSON Export (Complete)
- [x] Create `Adventure.BoxesLive` module
- [x] Implement `worldToCanvas` conversion
- [x] Create `realm-boxes-exporter` executable
- [x] Generate valid JSON schema
- [ ] **Test build:** `stack build`
- [ ] **Test export:** Run exporter on sample world

### 🔄 Phase 2: boxes-live Import (In Progress)
- [ ] Add JSON import to boxes-live
- [ ] Render boxes from imported data
- [ ] Show connections between rooms
- [ ] Highlight player location
- [ ] Test with realm-engine export

### 💡 Phase 3: Interactive Features
- [ ] Click to select room
- [ ] Show room details panel
- [ ] Navigate via connections
- [ ] Live updates (watch JSON file)

### 🚀 Phase 4: Deep Integration
- [ ] IPC for real-time updates
- [ ] Embed in adventure-tui
- [ ] Two-way communication
- [ ] Visual world editor

---

## File Locations

```
realm-engine/
├── src/
│   └── Adventure/
│       └── BoxesLive.hs          # Integration module
├── realm-boxes-exporter/
│   └── Main.hs                   # Exporter executable
├── docs/
│   └── BOXES-LIVE-INTEGRATION.md # This file
└── package.yaml                  # Added realm-boxes-exporter
```

---

## Testing Plan

### Unit Tests
```haskell
-- test/Adventure/BoxesLiveSpec.hs
describe "worldToCanvas" $ do
  it "converts world to canvas with correct box count" $ do
    let world = createTestWorld
    let canvas = worldToCanvas world
    length (canvasBoxes canvas) `shouldBe` 5

  it "marks player location correctly" $ do
    let world = createTestWorld
    let canvas = worldToCanvas world
    let playerBoxes = filter boxIsPlayerLocation (canvasBoxes canvas)
    length playerBoxes `shouldBe` 1
```

### Integration Tests
1. Load world from database
2. Export to JSON
3. Validate JSON schema
4. Re-import and verify structure

### Manual Testing
1. Create small test world (3-5 rooms)
2. Export to JSON
3. Inspect output: `jq . world.json`
4. Verify all rooms, connections, player location

---

## Dependencies

**realm-engine additions:**
- No new dependencies (uses existing `aeson`, `bytestring`, `postgresql-simple`)

**boxes-live additions (future):**
- JSON parsing library (e.g., `cJSON`, `json-c`)
- Dynamic data structures for imported boxes

---

## Benefits of Integration

1. **Enhanced Gameplay**
   - Players can see spatial relationships between rooms
   - Easier navigation in large worlds
   - Visual feedback for exploration

2. **Development Tool**
   - World designers can visualize layouts
   - Debug connectivity issues
   - Identify isolated areas

3. **Teaching/Demo**
   - Show game state visually
   - Explain game engine concepts
   - Live demonstrations of DSL features

4. **Cross-Project Synergy**
   - realm-engine provides rich game data
   - boxes-live provides interactive visualization
   - Both benefit from combined use case

---

## Resources

- **realm-engine:** https://github.com/jcaldwell1066/realm-engine
- **boxes-live:** https://github.com/jcaldwell1066/boxes-live
- **Workflow guide:** `.claude/CLAUDE-CODE-WORKFLOW.md`
- **Migration summary:** `MIGRATION-SUMMARY.md`

---

**Last updated:** 2025-11-16
**Status:** Phase 1 complete, awaiting build testing
