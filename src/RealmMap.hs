{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RealmMap where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

-- Room definition
data Room = Room
  { roomName :: String
  , roomDesc :: String
  , roomItems :: [String]
  , roomExits :: M.Map String String  -- direction -> destination
  } deriving (Show, Eq)

-- World state
data World = World
  { worldRooms :: M.Map String Room
  , playerLocation :: String
  , playerInventory :: [String]
  } deriving (Show, Eq)

-- Create the realm world
createRealm :: World
createRealm = World
  { worldRooms = M.fromList
      [ ("entrance", Room
          { roomName = "entrance"
          , roomDesc = "You are at the entrance of a mysterious realm."
          , roomItems = ["key", "map"]
          , roomExits = M.fromList [("north", "hallway"), ("east", "garden")]
          })
      , ("hallway", Room
          { roomName = "hallway"
          , roomDesc = "A long hallway stretches before you."
          , roomItems = ["torch"]
          , roomExits = M.fromList [("south", "entrance"), ("north", "chamber")]
          })
      , ("garden", Room
          { roomName = "garden"
          , roomDesc = "A beautiful garden with glowing flowers."
          , roomItems = ["crystal"]
          , roomExits = M.fromList [("west", "entrance")]
          })
      , ("chamber", Room
          { roomName = "chamber"
          , roomDesc = "The realm core chamber. You feel the DSL power here."
          , roomItems = ["realm_core"]
          , roomExits = M.fromList [("south", "hallway"), ("east", "exit_portal")]
          })
      , ("exit_portal", Room
          { roomName = "exit_portal"
          , roomDesc = "A shimmering portal beckons. The DSL adventure completes here!"
          , roomItems = []
          , roomExits = M.fromList [("west", "chamber")]
          })
      ]
  , playerLocation = "entrance"
  , playerInventory = []
  }

-- Render the map with player location highlighted
renderMap :: World -> String
renderMap world@World{..} = unlines
  [ "╭─────────────────────────────────────────────────────────────────╮"
  , "│                    REALM MAP - LIVE                             │"
  , "├─────────────────────────────────────────────────────────────────┤"
  , "│                                                                 │"
  , "│    " ++ roomBox "chamber" ++ "      " ++ roomBox "exit_portal" ++ "     │"
  , "│         " ++ connector "chamber" "│" ++ "═══════" ++ connector "exit_portal" "│" ++ "                │"
  , "│         " ++ connector "chamber" "↓" ++ "       " ++ connector "exit_portal" "↓" ++ "                │"
  , "│    " ++ roomBox "hallway" ++ "                                   │"
  , "│         " ++ connector "hallway" "│" ++ "                                        │"
  , "│         " ++ connector "hallway" "↓" ++ "                                        │"
  , "│    " ++ roomBox "entrance" ++ "      " ++ roomBox "garden" ++ "       │"
  , "│         " ++ connector "entrance" "│" ++ "═══════" ++ connector "garden" "│" ++ "                  │"
  , "│                                                                 │"
  , "├─────────────────────────────────────────────────────────────────┤"
  , "│ ╔═╗ = Your Location  │  ┌─┐ = Other Rooms  │  • = Items       │"
  , "╰─────────────────────────────────────────────────────────────────╯"
  ]
  where
    roomBox :: String -> String
    roomBox rname
      | rname == playerLocation = "╔═══════════╗"
      | otherwise = "┌───────────┐"

    roomLabel :: String -> String
    roomLabel rname
      | rname == playerLocation = "║►" ++ pad rname 8 ++ "◄║"
      | otherwise = "│ " ++ pad rname 9 ++ "│"

    connector :: String -> String -> String
    connector rname char
      | rname == playerLocation = "═"
      | otherwise = char

    pad :: String -> Int -> String
    pad s n = s ++ replicate (n - length s) ' '

-- Compact map rendering
renderCompactMap :: World -> String
renderCompactMap World{..} = unlines
  [ "Current Location: " ++ playerLocation
  , ""
  , "    [CHAMBER]───[EXIT_PORTAL]"
  , "        │"
  , "    [HALLWAY]"
  , "        │"
  , "    [ENTRANCE]───[GARDEN]"
  , ""
  , "Inventory: " ++ show playerInventory
  ]

-- ASCII art map with player avatar
renderAvatarMap :: World -> String
renderAvatarMap world@World{..} = unlines $
  [ "╭───────────────────────────────────────────────────────────────╮"
  , "│                  REALM - PLAYER VIEW                          │"
  , "├───────────────────────────────────────────────────────────────┤"
  , "│                                                               │"
  ] ++ mapLines ++
  [ "│                                                               │"
  , "├───────────────────────────────────────────────────────────────┤"
  , "│ @ = You  │  □ = Room  │  ◆ = Item  │  ★ = Goal               │"
  , "╰───────────────────────────────────────────────────────────────╯"
  ]
  where
    mapLines = case playerLocation of
      "entrance" ->
        [ "│         ┌─────────┐       ┌───────────┐                   │"
        , "│         │ CHAMBER │───────│EXIT_PORTAL│                   │"
        , "│         └────┬────┘       └───────────┘                   │"
        , "│              │                                            │"
        , "│         ┌────┴────┐                                       │"
        , "│         │ HALLWAY │                                       │"
        , "│         └────┬────┘                                       │"
        , "│              │                                            │"
        , "│         ╔════╩════╗       ┌───────────┐                   │"
        , "│         ║ @ HERE! ║───────│  GARDEN   │                   │"
        , "│         ║ENTRANCE ║       └───────────┘                   │"
        , "│         ╚═════════╝                                       │"
        ]
      "hallway" ->
        [ "│         ┌─────────┐       ┌───────────┐                   │"
        , "│         │ CHAMBER │───────│EXIT_PORTAL│                   │"
        , "│         └────┬────┘       └───────────┘                   │"
        , "│              │                                            │"
        , "│         ╔════╩════╗                                       │"
        , "│         ║ @ HERE! ║                                       │"
        , "│         ║ HALLWAY ║                                       │"
        , "│         ╚════╦════╝                                       │"
        , "│              │                                            │"
        , "│         ┌────┴────┐       ┌───────────┐                   │"
        , "│         │ENTRANCE │───────│  GARDEN   │                   │"
        , "│         └─────────┘       └───────────┘                   │"
        ]
      "chamber" ->
        [ "│         ╔═════════╗       ┌───────────┐                   │"
        , "│         ║ @ HERE! ║───────│EXIT_PORTAL│                   │"
        , "│         ║ CHAMBER ║       └───────────┘                   │"
        , "│         ╚════╦════╝                                       │"
        , "│              │                                            │"
        , "│         ┌────┴────┐                                       │"
        , "│         │ HALLWAY │                                       │"
        , "│         └────┬────┘                                       │"
        , "│              │                                            │"
        , "│         ┌────┴────┐       ┌───────────┐                   │"
        , "│         │ENTRANCE │───────│  GARDEN   │                   │"
        , "│         └─────────┘       └───────────┘                   │"
        ]
      "exit_portal" ->
        [ "│         ┌─────────┐       ╔═══════════╗                   │"
        , "│         │ CHAMBER │───────║ @ HERE!   ║                   │"
        , "│         └────┬────┘       ║EXIT_PORTAL║                   │"
        , "│              │             ╚═══════════╝                   │"
        , "│         ┌────┴────┐                                       │"
        , "│         │ HALLWAY │                                       │"
        , "│         └────┬────┘                                       │"
        , "│              │                                            │"
        , "│         ┌────┴────┐       ┌───────────┐                   │"
        , "│         │ENTRANCE │───────│  GARDEN   │                   │"
        , "│         └─────────┘       └───────────┘                   │"
        ]
      "garden" ->
        [ "│         ┌─────────┐       ┌───────────┐                   │"
        , "│         │ CHAMBER │───────│EXIT_PORTAL│                   │"
        , "│         └────┬────┘       └───────────┘                   │"
        , "│              │                                            │"
        , "│         ┌────┴────┐                                       │"
        , "│         │ HALLWAY │                                       │"
        , "│         └────┬────┘                                       │"
        , "│              │                                            │"
        , "│         ┌────┴────┐       ╔═══════════╗                   │"
        , "│         │ENTRANCE │───────║ @ HERE!   ║                   │"
        , "│         └─────────┘       ║  GARDEN   ║                   │"
        , "│                           ╚═══════════╝                   │"
        ]
      _ -> []