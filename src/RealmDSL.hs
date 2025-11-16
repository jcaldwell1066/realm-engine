{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RealmDSL where

import RealmMap
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
-- import System.Process (callCommand)  -- Hidden package, not needed for DSL demos
import System.IO (hFlush, stdout)
import Prelude hiding (take)

-- Game state (mutable reference in practice)
type GameState = World

-- DSL Commands
data Command
  = Look
  | Inventory
  | Take String
  | Drop String
  | Go String
  | ShowMap
  | Help
  | Quit
  deriving (Show, Eq)

-- Initialize game
initGame :: IO GameState
initGame = do
  putStrLn "╭─────────────────────────────────────────────────────╮"
  putStrLn "│       EPISODE 5: THE HASKELL DSL ADVENTURE          │"
  putStrLn "│                                                     │"
  putStrLn "│  Commands that validate AND document themselves!   │"
  putStrLn "│  Now in glorious Haskell!                          │"
  putStrLn "╰─────────────────────────────────────────────────────╯"
  putStrLn ""
  return createRealm

-- Look command
look :: GameState -> IO GameState
look world@World{..} = do
  let room = fromMaybe (error "Room not found") $ M.lookup playerLocation worldRooms
  putStrLn $ "┌─────────────────────────────────────┐"
  putStrLn $ "│ " ++ roomDesc room
  if not (null $ roomItems room)
    then putStrLn $ "│ Items: " ++ unwords (roomItems room)
    else putStrLn $ "│ Items: (none)"
  putStrLn $ "│ Exits: " ++ unwords (M.keys $ roomExits room)
  putStrLn $ "└─────────────────────────────────────┘"
  return world

-- Inventory command
inventory :: GameState -> IO GameState
inventory world@World{..} = do
  if null playerInventory
    then putStrLn "Your inventory is empty."
    else putStrLn $ "Carrying: " ++ unwords playerInventory
  return world

-- Take command with validation
take :: String -> GameState -> IO GameState
take item world@World{..} = do
  let room = fromMaybe (error "Room not found") $ M.lookup playerLocation worldRooms
  if item `elem` roomItems room
    then do
      putStrLn $ "✓ Validated: " ++ item ++ " exists"
      putStrLn $ "You took the " ++ item
      putStrLn $ "📝 Documented: take(" ++ item ++ ") at " ++ playerLocation

      -- Update room (remove item)
      let updatedRoom = room { roomItems = filter (/= item) (roomItems room) }
      let updatedRooms = M.insert playerLocation updatedRoom worldRooms

      return world
        { worldRooms = updatedRooms
        , playerInventory = item : playerInventory
        }
    else do
      putStrLn $ "✗ Validation failed: " ++ item ++ " not found here"
      return world

-- Go command with validation
go :: String -> GameState -> IO GameState
go direction world@World{..} = do
  let room = fromMaybe (error "Room not found") $ M.lookup playerLocation worldRooms
  case M.lookup direction (roomExits room) of
    Just newLocation -> do
      putStrLn $ "✓ Validated: " ++ direction ++ " is a valid exit"
      putStrLn $ "You go " ++ direction ++ "."
      putStrLn $ "📝 Documented: go(" ++ direction ++ ") " ++ playerLocation ++ " → " ++ newLocation

      let newWorld = world { playerLocation = newLocation }
      look newWorld
    Nothing -> do
      putStrLn $ "✗ Validation failed: Can't go " ++ direction ++ " from here"
      return world

-- Show map command
showMap :: GameState -> IO GameState
showMap world = do
  putStrLn $ renderMap world
  return world

-- Help command
help :: IO ()
help = do
  putStrLn "┌────────────────────────────────────────┐"
  putStrLn "│ DSL COMMANDS (Haskell REPL Edition)    │"
  putStrLn "├────────────────────────────────────────┤"
  putStrLn "│ look           - examine room          │"
  putStrLn "│ inventory      - check items           │"
  putStrLn "│ take \"item\"    - pick up item          │"
  putStrLn "│ go \"direction\" - move around           │"
  putStrLn "│ showMap        - display realm map     │"
  putStrLn "│ help           - this message          │"
  putStrLn "│                                        │"
  putStrLn "│ In ghci, bind to world:                │"
  putStrLn "│   world <- look world                  │"
  putStrLn "│   world <- take \"key\" world            │"
  putStrLn "│   world <- go \"north\" world            │"
  putStrLn "│   showMap world                        │"
  putStrLn "└────────────────────────────────────────┘"

-- Convenience functions for REPL
lookCmd :: GameState -> IO GameState
lookCmd = look

inv :: GameState -> IO GameState
inv = inventory

takeCmd :: String -> GameState -> IO GameState
takeCmd = take

goCmd :: String -> GameState -> IO GameState
goCmd = go

mapCmd :: GameState -> IO GameState
mapCmd = showMap