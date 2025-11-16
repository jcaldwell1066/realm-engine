{-# LANGUAGE OverloadedStrings #-}

module Adventure.Engine.Database.Loader
  ( loadWorld
  , loadWorldByName
  , LoaderError(..)
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Monad (forM)

import Adventure.Engine (World(..), VerbAliasMap, _worldRooms, _worldObjects, _worldExits, _worldPlayerRoom, _worldPlayerInventory, _worldPlayerVerbs, _worldPlayerMessages)
import Adventure.Engine.Objects
import Adventure.Engine.Database (EntityId(..))
import Adventure.Engine.Database.Connection (DbConfig, withConnection)

-- | Errors that can occur during world loading
data LoaderError
  = WorldNotFound Int
  | PlayerStateNotFound Int
  | DatabaseError String
  deriving (Show, Eq)

-- | Load a World from the database by world ID
loadWorld :: Connection -> Int -> IO (Maybe World)
loadWorld conn worldId = do
  -- Load all entities
  rooms <- loadRooms conn worldId
  gameObjects <- loadGameObjects conn worldId
  exits <- loadExits conn worldId
  playerState <- loadPlayerState conn worldId

  case playerState of
    Nothing -> return Nothing
    Just (playerRoom, playerInv, playerVerbs) ->
      return $ Just $ World
        { _worldRooms = M.fromList rooms
        , _worldObjects = M.fromList gameObjects
        , _worldExits = M.fromList exits
        , _worldPlayerRoom = playerRoom
        , _worldPlayerInventory = playerInv
        , _worldPlayerVerbs = playerVerbs
        , _worldPlayerMessages = []
        }

-- | Load a World from the database by world name
loadWorldByName :: DbConfig -> T.Text -> IO (Maybe World)
loadWorldByName config worldName =
  withConnection config $ \conn -> do
    -- For now, we'll use world_id = 1 (single world)
    -- In future, query worlds table to get ID by name
    loadWorld conn 1

-- Database row types and loaders

-- Load Rooms
loadRooms :: Connection -> Int -> IO [(EntityId Room, Room)]
loadRooms conn worldId = do
  rows <- query conn
    "SELECT id, name, description, background FROM rooms WHERE world_id = ?"
    (Only worldId) :: IO [(Int, T.Text, T.Text, Maybe T.Text)]

  forM rows $ \(roomId, name, desc, bg) -> do
    -- Load room exits
    exitIds <- query conn
      "SELECT exit_id, exit_name FROM room_exits WHERE room_id = ?"
      (Only roomId) :: IO [(Int, T.Text)]
    let exitsMap = M.fromList [(exitName, EntityId exitId) | (exitId, exitName) <- exitIds]

    -- Load room objects
    objIds <- query conn
      "SELECT game_object_id, object_name FROM room_objects WHERE room_id = ?"
      (Only roomId) :: IO [(Int, T.Text)]
    let objectsMap = M.fromList [(objName, EntityId objId) | (objId, objName) <- objIds]

    -- Load dig definitions
    digDefs <- query conn
      "SELECT success_msg, game_object_id FROM dig_definitions WHERE room_id = ? ORDER BY id"
      (Only roomId) :: IO [(T.Text, Maybe Int)]
    let digResult = if null digDefs
          then Left "You can't dig here."
          else Right [DigDefinition success (fmap EntityId objId) | (success, objId) <- digDefs]

    return (EntityId roomId, Room name desc objectsMap exitsMap digResult bg)

-- Load GameObjects
loadGameObjects :: Connection -> Int -> IO [(EntityId GameObject, GameObject)]
loadGameObjects conn worldId = do
  rows <- query conn
    "SELECT id, name, description, object_type, size, weight FROM game_objects WHERE world_id = ?"
    (Only worldId) :: IO [(Int, T.Text, T.Text, T.Text, Maybe Int, Maybe Int)]

  forM rows $ \(objId, name, desc, objType, size, weight) -> do
    obj <- case objType of
      "item" -> do
        -- For now, items have no custom verbs
        return $ ObjectItem $ Item (maybe 0 id size) (maybe 0 id weight) []

      "container" -> do
        -- Load container items
        items <- query conn
          "SELECT game_object_id, item_name FROM container_items WHERE container_id = ?"
          (Only objId) :: IO [(Int, T.Text)]
        let itemsMap = M.fromList [(itemName, EntityId itemId) | (itemId, itemName) <- items]

        -- Load container lock if exists
        lockData <- query conn
          "SELECT lock_state, fail_msg, success_msg, unlocked_desc FROM game_objects WHERE id = ? AND lock_state IS NOT NULL"
          (Only objId) :: IO [(T.Text, T.Text, T.Text, Maybe T.Text)]

        containerLock <- case lockData of
          [] -> return Nothing
          [(lockState, failMsg, successMsg, unlockedDesc)] -> do
            -- Load lock keys
            keys <- query conn
              "SELECT key_game_object_id FROM container_lock_keys WHERE container_id = ?"
              (Only objId) :: IO [Only Int]
            let keyIds = [EntityId keyId | Only keyId <- keys]
            let state = if lockState == "locked" then Locked else Unlocked
            let lock = Lock keyIds failMsg successMsg state
            return $ Just $ ContainerLock lock (maybe "" id unlockedDesc)
          _ -> return Nothing

        return $ ObjectContainer $ Container (maybe 0 id size) itemsMap containerLock

      _ -> return $ ObjectItem $ Item 0 0 []

    return (EntityId objId, GameObject name desc obj)

-- Load Exits
loadExits :: Connection -> Int -> IO [(EntityId Exit, Exit)]
loadExits conn worldId = do
  rows <- query conn
    "SELECT id, name, description, from_room_id, to_room_id, lock_state, fail_msg, success_msg FROM exits WHERE world_id = ?"
    (Only worldId) :: IO [(Int, T.Text, T.Text, Int, Int, Maybe T.Text, Maybe T.Text, Maybe T.Text)]

  forM rows $ \(exitId, name, desc, fromId, toId, lockState, failMsg, successMsg) -> do
    exitLock <- case lockState of
      Nothing -> return Nothing
      Just state -> do
        -- Load exit lock keys
        keys <- query conn
          "SELECT key_game_object_id FROM exit_lock_keys WHERE exit_id = ?"
          (Only exitId) :: IO [Only Int]
        let keyIds = [EntityId keyId | Only keyId <- keys]
        let st = if state == "locked" then Locked else Unlocked
        return $ Just $ Lock keyIds (maybe "" id failMsg) (maybe "" id successMsg) st

    return (EntityId exitId, Exit name desc (EntityId fromId) (EntityId toId) exitLock)

-- Load Player State
loadPlayerState :: Connection -> Int -> IO (Maybe (EntityId Room, M.Map T.Text (EntityId GameObject), VerbAliasMap))
loadPlayerState conn worldId = do
  playerRows <- query conn
    "SELECT current_room_id FROM player_state WHERE world_id = ? LIMIT 1"
    (Only worldId) :: IO [Only Int]

  case playerRows of
    [] -> return Nothing
    (Only roomId:_) -> do
      -- Load player inventory
      invRows <- query conn
        "SELECT item_name, game_object_id FROM player_inventory WHERE world_id = ?"
        (Only worldId) :: IO [(T.Text, Int)]
      let inventory = M.fromList [(itemName, EntityId objId) | (itemName, objId) <- invRows]

      -- Load player verbs (for now, empty - verb system TBD)
      let verbs = M.empty :: VerbAliasMap

      return $ Just (EntityId roomId, inventory, verbs)
