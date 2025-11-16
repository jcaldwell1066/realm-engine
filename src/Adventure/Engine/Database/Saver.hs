{-# LANGUAGE OverloadedStrings #-}

module Adventure.Engine.Database.Saver
  ( savePlayerState
  , SaverError(..)
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Database.PostgreSQL.Simple
import Control.Monad (forM_)

import Adventure.Engine (World(..), GameState(..), _gameStateWorld, _worldPlayerRoom, _worldPlayerInventory)
import Adventure.Engine.Database (EntityId(..))

-- | Errors that can occur during save operations
data SaverError
  = SaveFailed String
  | DatabaseWriteError String
  deriving (Show, Eq)

-- | Save the current player state to the database
-- Updates player_state, player_inventory, and player_verbs tables
-- Assumes world_id = 1 (single world for now)
savePlayerState :: Connection -> GameState -> IO ()
savePlayerState conn gameState = do
  let world' = _gameStateWorld gameState
      worldId = 1 :: Int
      EntityId roomId = _worldPlayerRoom world'
      inventory = _worldPlayerInventory world'

  -- Begin transaction
  _ <- execute_ conn "BEGIN"

  -- Update player's current room
  _ <- execute conn
    "UPDATE player_state SET current_room_id = ? WHERE world_id = ?"
    (roomId, worldId)

  -- Clear existing inventory
  _ <- execute conn
    "DELETE FROM player_inventory WHERE world_id = ?"
    (Only worldId)

  -- Insert current inventory items
  forM_ (M.toList inventory) $ \(itemName, EntityId objId) -> do
    _ <- execute conn
      "INSERT INTO player_inventory (world_id, item_name, game_object_id) VALUES (?, ?, ?)"
      (worldId, itemName, objId)
    return ()

  -- Commit transaction
  _ <- execute_ conn "COMMIT"

  return ()

-- | Quick save - saves without returning anything
quickSave :: Connection -> GameState -> IO ()
quickSave = savePlayerState
