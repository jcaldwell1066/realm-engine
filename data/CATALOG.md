# Realm Engine Catalog

This directory contains a growing collection of realm world files for the Realm Engine. Each realm is a complete, playable adventure with its own story, puzzles, and mechanics.

## Catalog Structure

### Master Catalog
- **`catalog.json`**: Master index of all realms with metadata

### Realm Directories
Each realm is stored in its own directory under `data/` with the following structure:

```
data/
├── catalog.json                    # Master catalog index
├── CATALOG.md                      # This file
├── [realm-id]/                     # Individual realm directory
│   ├── metadata.json               # Realm-specific metadata
│   ├── world.txt                   # DSL source definition
│   ├── world.json                  # JSON export
│   ├── world.sql                   # SQL schema export
│   ├── event_rewards.json          # Event-based scoring
│   ├── game_end_rewards.json       # Win conditions
│   └── build/                      # Build artifacts
```

## Realm Metadata Schema

Each realm's `metadata.json` contains:

| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Unique identifier (matches directory name) |
| `name` | string | Display name |
| `description` | string | Brief description of the realm |
| `category` | string | Category: `tutorial`, `team-building`, `adventure`, `puzzle`, `challenge` |
| `difficulty` | string | Difficulty level: `beginner`, `intermediate`, `advanced` |
| `estimatedMinutes` | number | Estimated completion time in minutes |
| `rooms` | number | Number of rooms in the realm |
| `objects` | number | Number of game objects (items + containers) |
| `author` | string | Creator name |
| `version` | string | Semantic version (e.g., "1.0.0") |
| `created` | string | Creation date (ISO 8601 format) |
| `tags` | string[] | Search tags |
| `features` | string[] | Game mechanics used |
| `learningObjectives` | string[] | What players will learn/practice |
| `walkthrough` | string[] | (Optional) Step-by-step solution |

## Categories

- **tutorial**: Introductory realms teaching core mechanics
- **team-building**: Collaborative experiences focusing on teamwork skills
- **adventure**: Story-driven exploration and puzzle-solving
- **puzzle**: Logic and problem-solving focused
- **challenge**: Advanced difficulty, timed, or competitive scenarios

## Features Taxonomy

Common features to tag realms with:

- `locked-doors` - Doors requiring keys
- `containers` - Chests, boxes, etc.
- `hidden-items` - Items discovered through digging or searching
- `digging` - Ground-based item discovery
- `key-collection` - Multiple keys to manage
- `combination-locks` - Numeric or code-based locks
- `item-placement` - Puzzles requiring items in specific locations
- `event-triggers` - Actions that trigger world changes
- `timed-challenges` - Time-sensitive puzzles
- `multiple-solutions` - Puzzles with various valid approaches

## Adding a New Realm

1. Create a new directory: `data/[realm-id]/`
2. Create the realm using the DSL in `world.txt`
3. Generate `world.json` and `world.sql` (using build tools)
4. Create `metadata.json` with complete information
5. Add entry to `data/catalog.json`
6. Define win conditions in `game_end_rewards.json`
7. (Optional) Add event scoring in `event_rewards.json`

## Querying the Catalog

Use the `Catalog` Haskell module to:
- List all realms
- Filter by category, difficulty, or tags
- Get realm metadata
- Validate catalog consistency

Example:
```haskell
import Catalog

main :: IO ()
main = do
  catalog <- loadCatalog "data/catalog.json"
  let teamBuilding = filterByCategory "team-building" catalog
  mapM_ printRealmInfo teamBuilding
```

## Current Realms

See `catalog.json` for the complete, up-to-date list of available realms.
