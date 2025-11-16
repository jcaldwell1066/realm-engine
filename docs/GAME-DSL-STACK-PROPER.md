+--------------------------------------------------------------------------+
|                                                                          |
| GAME DSL STACK REVIEW - FROM FEEDBACK LOOP UP                            |
|                                                                          |
| THE CONFUSION:                                                           |
| Python .realm files are a TESTING WRAPPER, not the core DSL              |
| The REAL game DSL is Haskell-based with Megaparsec                       |
|                                                                          |
| THE PROPER STACK:                                                        |
|                                                                          |
| ┌─────────────────────────────────────────────────────────────┐          |
| │ LEVEL 1: WORLD FILES (world.txt)                           │           |
| │                                                             │          |
| │ Format:                                                     │          |
| │   [GameObjects]                                             │          |
| │   # 1. Rusty Key                                            │          |
| │   A rusty old key...                                        │          |
| │   #                                                         │          |
| │   -- Type: Item                                             │          |
| │   -- Size: 1                                                │          |
| │                                                             │          |
| │   [Rooms]                                                   │          |
| │   # 1. The Front Porch                                      │          |
| │   You stand on a porch...                                   │          |
| │   #                                                         │          |
| │   -- Exits: (1. Front Door)                                 │          |
| │   -- Objects: (1. Rusty Key)                                │          |
| │                                                             │          |
| │   [Exits]                                                   │          |
| │   # 1. Front Door                                           │          |
| │   A heavy oak door...                                       │          |
| │   #                                                         │          |
| │   -- From: (1. The Front Porch)                             │          |
| │   -- To: (2. Main Hallway)                                  │          |
| │   -- Lock:                                                  │          |
| │   --   Keys: (1. Rusty Key)                                 │          |
| │                                                             │          |
| │ Files: data/example-adventure/world.txt (158 lines)         │          |
| │        data/condo-world/world.txt (174 lines)               │          |
| │        examples/payment-api.world (316 lines)               │          |
| │                                                             │          |
| │ ↓↓↓ Parsed by Megaparsec ↓↓↓                                │          |
| └─────────────────────────────────────────────────────────────┘          |
|                                                                          |
| ┌─────────────────────────────────────────────────────────────┐          |
| │ LEVEL 2: MEGAPARSEC PARSER (Maker.Parser.hs)               │           |
| │                                                             │          |
| │ Parsers:                                                    │          |
| │   worldParser :: Parser World                               │          |
| │   roomParser :: Parser (EntityId Room, Room)                │          |
| │   exitParser :: Parser (EntityId Exit, Exit)                │          |
| │   gameObjectParser :: Parser (EntityId GameObject, ...)     │          |
| │   verbParser :: Parser Verb                                 │          |
| │                                                             │          |
| │ Result type:                                                │          |
| │   Either ParseError Adventure.Engine.World                  │          |
| │                                                             │          |
| │ Status: ✅ Successfully parsed world.txt                    │          |
| │                                                             │          |
| │ ↓↓↓ Produces typed Haskell data ↓↓↓                         │          |
| └─────────────────────────────────────────────────────────────┘          |
|                                                                          |
| ┌─────────────────────────────────────────────────────────────┐          |
| │ LEVEL 3: GAME ENGINE DATA (Adventure.Engine)               │           |
| │                                                             │          |
| │ Core types:                                                 │          |
| │   data World = World                                        │          |
| │     { _worldRooms :: Map (EntityId Room) Room               │          |
| │     , _worldExits :: Map (EntityId Exit) Exit               │          |
| │     , _worldGameObjects :: Map (EntityId GameObject) ...    │          |
| │     , _worldPlayerRoom :: EntityId Room                     │          |
| │     , _worldPlayerInventory :: Set (EntityRef GameObject)   │          |
| │     , _worldPlayerVerbs :: Map Verb Verb                    │          |
| │     }                                                       │          |
| │                                                             │          |
| │   data Room = Room                                          │          |
| │     { _roomName :: Text                                     │          |
| │     , _roomDescription :: Text                              │          |
| │     , _roomExits :: Set (EntityRef Exit)                    │          |
| │     , _roomObjects :: Set (EntityRef GameObject)            │          |
| │     }                                                       │          |
| │                                                             │          |
| │ This is TYPED, IMMUTABLE game state in Haskell              │          |
| │                                                             │          |
| │ ↓↓↓ Manipulated by game logic ↓↓↓                           │          |
| └─────────────────────────────────────────────────────────────┘          |
|                                                                          |
| ┌─────────────────────────────────────────────────────────────┐          |
| │ LEVEL 4: GAME LOGIC / DSL COMMANDS (RealmDSL.hs)           │           |
| │                                                             │          |
| │ Commands (the actual DSL):                                  │          |
| │   look :: GameState -> IO GameState                         │          |
| │   take :: String -> GameState -> IO GameState               │          |
| │   go :: String -> GameState -> IO GameState                 │          |
| │   inventory :: GameState -> IO GameState                    │          |
| │   drop :: String -> GameState -> IO GameState               │          |
| │                                                             │          |
| │ Type: GameState = World                                     │          |
| │                                                             │          |
| │ These functions ARE the game DSL interpreter                │          |
| │                                                             │          |
| │ ↓↓↓ Executes and produces output ↓↓↓                        │          |
| └─────────────────────────────────────────────────────────────┘          |
|                                                                          |
| ┌─────────────────────────────────────────────────────────────┐          |
| │ LEVEL 5: PLAYER INTERACTION / FEEDBACK LOOP                │           |
| │                                                             │          |
| │ Input: Player types "take key"                              │          |
| │   ↓                                                         │          |
| │ Parser: Parses to Take "key" command                        │          |
| │   ↓                                                         │          |
| │ Execute: take "key" currentWorld                            │          |
| │   ↓                                                         │          |
| │ Output: "You took the key"                                  │          |
| │   ↓                                                         │          |
| │ New State: World with updated inventory                     │          |
| │   ↓                                                         │          |
| │ Feedback: Success/failure informs next action               │          |
| │   ↑───────────────────────────────────────────┐             │          |
| │   └ Loop: Next command uses updated state ────┘             │          |
| └─────────────────────────────────────────────────────────────┘          |
|                                                                          |
| WHERE PYTHON .realm FILES FIT:                                           |
| ❌ NOT part of the core DSL                                              |
| ✅ Testing/demo wrapper for the Haskell DSL                              |
| ✅ Used for documentation and validation                                 |
| ✅ Can invoke the Haskell DSL via [Haskell] blocks                       |
|                                                                          |
| CURRENTLY IN GHCI:                                                       |
| Parsed: data/example-adventure/world.txt                                 |
| Result: Right World { 6 rooms, 10 exits, 4 objects }                     |
|                                                                          |
+--------------------------------------------------------------------------+
