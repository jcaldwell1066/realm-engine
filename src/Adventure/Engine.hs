{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Adventure.Engine
  ( module Adventure.Engine
  , module Adventure.Engine.Objects
  )
where

import Codec.Archive.Zip
import qualified Codec.Picture as Pic
import Control.Exception
import Control.Error (note)
import Control.Lens hiding ((.=))
import Control.Monad (forM, when)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Vector.Storable.ByteString (vectorToByteString)
import GHC.Generics
import System.Console.Haskeline
import System.Directory
import System.FilePath

import Adventure.Data.Map.Util
import Adventure.Engine.Database
import Adventure.Engine.Language
import Adventure.Engine.ImageData
import Adventure.Engine.Objects
import Adventure.Engine.Rewards
import Adventure.List.Utils

itemObjectVerbAliasMap :: GameObject -> Maybe (Map Verb Verb)
itemObjectVerbAliasMap (GameObject _ _ (ObjectItem item'))
  = pure . M.fromList . _itemVerbs $ item'
itemObjectVerbAliasMap _ = Nothing

type VerbAliasMap = Map Verb Verb

data Scene
  = Scene
  { _sceneTitle       :: Text
  , _sceneDescription :: Text
  , _sceneObjects     :: [GameObject]
  , _sceneExits       :: [Exit]
  , _sceneMessages    :: [Text]
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON Scene
deriving instance JSON.FromJSON Scene

data World
  = World
  { _worldRooms       :: Map (EntityId Room) Room
  , _worldObjects     :: Map (EntityId GameObject) GameObject
  , _worldExits       :: Map (EntityId Exit) Exit
  , _worldPlayerRoom  :: EntityId Room
  , _worldPlayerInventory  :: Map Text (EntityId GameObject)
  , _worldPlayerVerbs      :: VerbAliasMap
  , _worldPlayerMessages :: [Text]
  }
  deriving (Eq, Show)

instance JSON.ToJSON World where
  toJSON (World rooms objects exits pRoom pInv pVerbs _) =
    JSON.object
    [ "rooms"           .= rooms
    , "objects"         .= objects
    , "exits"           .= exits
    , "playerRoom"      .= pRoom
    , "playerInventory" .= pInv
    , "playerVerbs"     .= pVerbs
    ]

instance JSON.FromJSON World where
  parseJSON = JSON.withObject "World" $ \v ->
    World
    <$> v .: "rooms"
    <*> v .: "objects"
    <*> v .: "exits"
    <*> v .: "playerRoom"
    <*> v .: "playerInventory"
    <*> v .: "playerVerbs"
    <*> pure mempty

makeFields ''World


-- TODO (james): a better way to show errors
data GameState
  = GameState
  { _gameStateWorld          :: World
  , _gameStateScenes         :: [Scene]
  , _gameStateInputBuffer    :: Text
  , _gameStateGameErrors     :: [Text]
  , _gameStateEventLog       :: [Event]
  , _gameStateRewards        :: [EventReward]
  , _gameStateGameEndRewards :: NonEmpty GameEndReward
  , _gameStateIsGameEnd      :: Maybe GameEndReward
  , _gameStateImageCache     :: Map Text ImageData
  }
  deriving (Eq, Show)

makeFields ''GameState

instance JSON.ToJSON GameState where
  toJSON gameState =
    JSON.object
    [ "world" .= (gameState ^. world)
    , "scenes" .= (gameState ^. scenes)
    , "events" .= (gameState ^. eventLog)
    , "rewards" .= (gameState ^. rewards)
    , "gameEndRewards" .= (gameState ^. gameEndRewards)
    , "gameEnd" .= (gameState ^. isGameEnd)
    ]

instance JSON.FromJSON GameState where
  parseJSON = JSON.withObject "GameState" $ \v -> do
    gameStateGameEndRewards <- v .: "gameEndRewards"
    if null gameStateGameEndRewards
      then fail "Empty game end rewards detected when loading game state"
      else GameState
           <$> v .: "world"
           <*> v .: "scenes"
           <*> pure mempty
           <*> pure mempty
           <*> v .: "events"
           <*> v .: "rewards"
           <*> (pure . NE.fromList $ gameStateGameEndRewards)
           <*> v .: "gameEnd"
           <*> pure mempty

fetch :: (Monad m, Ord k) => GameError -> k -> Map k v -> ExceptT GameError m v
fetch e key db = maybe (throwError e) pure $ M.lookup key db

-- Case-insensitive lookup for Text keys
fetchText :: Monad m => GameError -> Text -> Map Text v -> ExceptT GameError m v
fetchText e key db =
  case lookupTextCI key db of
    Just v  -> pure v
    Nothing -> throwError e

-- Case-insensitive Map lookup helper
lookupTextCI :: Text -> Map Text v -> Maybe v
lookupTextCI key db =
  let keyLower = T.toLower key
      matchingKey = find (\k -> T.toLower k == keyLower) (M.keys db)
  in matchingKey >>= \k -> M.lookup k db

currentRoom :: Monad m => World -> ExceptT GameError m Room
currentRoom w = fetch SpaceWizard (_worldPlayerRoom w) (_worldRooms w)

exitCurrentRoom
  :: ( MonadState GameState m, Monad m )
  => Text -> ExceptT GameError m (EntityId Exit, Exit)
exitCurrentRoom exitName = do
  (GameState w _ _ _ _ _ _ _ _) <- lift get
  room <- currentRoom w
  exitId <- fetchText (ExitDoesNotExist' exitName) exitName (_roomExits room)
  exit <- fetch SpaceWizard exitId (_worldExits w)
  pure (exitId, exit)

getObjectInCurrentRoom
  :: Monad m
  => World
  -> Text
  -> ExceptT GameError m (EntityId GameObject)
getObjectInCurrentRoom w objectName = do
  room <- currentRoom w
  fetchText (ObjectNotInRoom objectName) objectName $ _roomObjects room

updateCurrentRoom
  :: ( Monad m, MonadState GameState m )
  => (Room -> Room)
  -> ExceptT GameError m ()
updateCurrentRoom update = do
  gameState <- lift get
  let world'  = gameState ^. world
  lift $ put gameState
    { _gameStateWorld = world'
      { _worldRooms = M.update (Just . update) (_worldPlayerRoom world') (world' ^. rooms)
      }
    }

getObjectInInventory
  :: Monad m
  => World
  -> Text
  -> ExceptT GameError m (EntityId GameObject)
getObjectInInventory w objectName =
  fetchText (ObjectNotInInventory objectName) objectName $ _worldPlayerInventory w

getInventoryObjects :: Monad m => World -> ExceptT GameError m [GameObject]
getInventoryObjects w = traverse (getObject w) $ M.elems (_worldPlayerInventory w)

getObjectByName
  :: ( MonadState GameState m, Monad m)
  => Text
  -> ExceptT GameError m (EntityId GameObject, GameObject)
getObjectByName objectName = do
  gameState <- lift get
  objectId <- maybeThrow (ObjectNotInInventory objectName) $
    M.lookup objectName (_worldPlayerInventory $ gameState ^. world)
  object <- maybeThrow (ObjectNotInInventory objectName) $
    M.lookup objectId (_worldObjects $ gameState ^. world)
  pure (objectId, object)

putObjectInInventory
  :: Text
  -> EntityId GameObject
  -> Map Text (EntityId GameObject)
  -> Map Text (EntityId GameObject)
putObjectInInventory objectName = M.insert (T.toLower objectName)

getObject :: Monad m => World -> EntityId GameObject -> ExceptT GameError m GameObject
getObject w objectId =
  fetch (ObjectDoesNotExist objectId) objectId $ _worldObjects w

modifyObject
  :: Monad m
  => World
  -> EntityId GameObject
  -> (GameObject -> GameObject)
  -> ExceptT GameError m World
modifyObject world' objectId f =
  pure $ world' { _worldObjects = M.adjust f objectId (world' ^. objects) }

getExitId :: Monad m => World -> Text -> ExceptT GameError m (EntityId Exit)
getExitId world' exitName =
  liftEither
  $ note SpaceWizard
  $ findInMap (\e -> _exitName e == exitName) (world' ^. exits)

modifyExit
  :: Monad m
  => World
  -> EntityId Exit
  -> (Exit -> Exit)
  -> ExceptT GameError m World
modifyExit world' exitId f =
  pure $ world' { _worldExits = M.adjust f exitId (world' ^. exits) }

-- TODO (james): Might replace this with accessors into some kind of player state record
getPlayerInventory :: GameState -> [GameObject]
getPlayerInventory gameState =
  case runExcept $ getInventoryObjects $ _gameStateWorld gameState of
    Left err -> error $ "Error getting inventory: " ++ show err
    Right objects' -> objects'

putInInventory
  :: ( Monad m, MonadState GameState m )
  => Text
  -> EntityId GameObject
  -> ExceptT GameError m ()
putInInventory objectName objectId = do
  gameState <- lift get
  let playerInv = gameState ^. (world . playerInventory)
  lift . put $ gameState
    { _gameStateWorld = (gameState ^. world)
      { _worldPlayerInventory = putObjectInInventory objectName objectId playerInv
      }
    }

addPlayerMessage
  :: ( MonadState GameState m, Monad m )
  => Text
  -> ExceptT GameError m ()
addPlayerMessage msg = do
  gameState <- lift get
  let world' = gameState ^. world
  -- TODO (james): clean this up with `set`
  lift . put $ gameState { _gameStateWorld = world' { _worldPlayerMessages = msg : _worldPlayerMessages world' } }

setGameEnd
  :: ( Monad m, MonadState GameState m )
  => GameEndReward
  -> ExceptT GameError m ()
setGameEnd gameEndReward = do
  g <- lift get
  lift . put $ g { _gameStateIsGameEnd = Just gameEndReward }

getCurrentBackground :: GameState -> Maybe (Text, ImageData)
getCurrentBackground gameState =
  let world' = gameState ^. world
  in case M.lookup (_worldPlayerRoom world') (_worldRooms world') of
    Nothing -> Nothing
    Just r  -> do
      imgName <- _roomBackground r
      imageData <- M.lookup imgName $ _gameStateImageCache gameState
      pure (imgName, imageData)

emitEvent :: (Monad m, MonadState GameState m) => Event -> ExceptT GameError m ()
emitEvent event =
  lift
  $ modify ( \gameState ->
               gameState { _gameStateEventLog = (gameState ^. eventLog) ++ [event]}
           )

newtype GameEngine m a = GameEngine { runEngine :: ExceptT GameError (StateT GameState m) a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadState GameState
    , MonadError GameError
    )

-- Managing the World

defaultVerbAliasMap :: VerbAliasMap
defaultVerbAliasMap
  = M.fromList
  [ (Verb "l", Verb "look")
  , (Verb "look", Verb "look")
  , (Verb "w", Verb "walk")
  , (Verb "walk", Verb "walk")
  , (Verb "p", Verb "pickup")
  , (Verb "pickup", Verb "pickup")
  , (Verb "d", Verb "drop")
  , (Verb "drop", Verb "drop")
  , (Verb "e", Verb "examine")
  , (Verb "examine", Verb "examine")
  , (Verb "t", Verb "take")
  , (Verb "take", Verb "take")
  , (Verb "put", Verb "put")
  , (Verb "use", Verb "use")
  , (Verb "u", Verb "use")
  , (Verb "dig", Verb "dig")
  , (Verb "inventory", Verb "inventory")
  , (Verb "i", Verb "inventory")
  , (Verb "help", Verb "help")
  , (Verb "h", Verb "help")
  , (Verb "open", Verb "open")
  , (Verb "o", Verb "open")
  ]

getVerb :: Monad m => VerbAliasMap -> Verb -> ExceptT GameError m Verb
getVerb verbAliasMap v =
  maybeThrow (UnrecognizedVerb v) $ M.lookup v verbAliasMap

parse :: Monad m => Text -> ExceptT GameError m (Verb, [Text])
parse = parseCmd . T.words
  where
    parseCmd :: Monad m => [Text] -> ExceptT GameError m (Verb, [Text])
    parseCmd [] = throwError MissingCommand
    parseCmd (x:xs) = pure (Verb x, xs)

defaultGameState :: World -> NonEmpty GameEndReward -> GameState
defaultGameState world' gameEndRewards' =
  GameState
  world'
  []
  ""
  []
  []
  []
  gameEndRewards'
  Nothing
  mempty

initialGameState :: World -> [EventReward] -> NonEmpty GameEndReward -> Either GameError GameState
initialGameState world' eventRewards gameEndRewards' = do
  initialScene <- runExcept $ render world'
  pure
    $ (defaultGameState world' gameEndRewards')
    { _gameStateScenes = [initialScene]
    , _gameStateRewards = eventRewards
    }

checkGameEnd
  :: ( Monad m, MonadState GameState m )
  => ExceptT GameError m ()
checkGameEnd = do
  g <- lift get
  case _gameStateEventLog g of
    [] -> pure ()  -- No events yet, can't end game
    events -> case gameEnd (last events) (_gameStateGameEndRewards g) of
      Nothing -> pure ()
      Just gameOverText -> setGameEnd gameOverText

handle' :: (Monad m, MonadState GameState m) => ExceptT GameError m ()
handle' = do
  gameState <- lift get
  (v, args) <- parse $ gameState ^. inputBuffer
  verb <- getVerb (_worldPlayerVerbs $ gameState ^. world) v
  handleVerb verb args
  checkGameEnd

data GameError
  = RoomDoesNotExist (EntityId Room)
  | ObjectDoesNotExist (EntityId GameObject)
  | ObjectNotInRoom Text
  | ObjectNotInInventory Text
  | ObjectNotInContainer Text
  | InvalidObjectParameter Text
  | InvalidObjectInteraction Text Text Command
  | ExitDoesNotExist (EntityId Exit)
  | ExitDoesNotExist' Text
  | MissingCommand
  | InvalidCommand Text
  | UnrecognizedVerb Verb
  | MissingParameter Text
  | SaveFileError Text
  | SaveLoadError Text
  | SpaceWizard
  deriving (Eq, Show)

instance Exception GameError

gameErrorText :: GameError -> Text
gameErrorText = \case
  RoomDoesNotExist _         -> "That room does not exist."
  ObjectDoesNotExist _       -> "That object does not exist."
  ObjectNotInRoom txt        -> "I don't see '" <> txt <> "' here."
  ObjectNotInInventory txt   -> "You are not holding, '" <> txt <> "'."
  ObjectNotInContainer txt   -> "I don't see '" <> txt <> "' in there."
  InvalidObjectParameter txt -> "You can't really do that with, '" <> txt <> "'."
  InvalidObjectInteraction s p _ -> "I don't know how to use " <> s <> " on " <> p
  InvalidCommand txt         -> txt
  ExitDoesNotExist _         -> "I don't see that exit here."
  ExitDoesNotExist' txt      -> "I don't see an exit called, '" <> txt <> "' here."
  MissingCommand             -> "Did you mean to give me a command?"
  UnrecognizedVerb v         -> "I don't know what you mean by, '" <> unVerb v <> "'."
  MissingParameter txt       -> "I'm missing more information about, '" <> txt <> "'."
  SaveLoadError txt          -> "Error loading save file: " <> txt
  SaveFileError txt          -> "Error saving file: " <> txt
  SpaceWizard                -> "SPACE WIZARD!!!!"

handleVerb :: (Monad m, MonadState GameState m) => Verb -> [Text] -> ExceptT GameError m ()
handleVerb (Verb "walk") args = handleWalk args
handleVerb (Verb "pickup") args = handlePickup args
handleVerb (Verb "drop") args = handleDrop args
handleVerb (Verb "look") args = handleLook args
handleVerb (Verb "examine") args = handleExamine args
handleVerb (Verb "take") args = handleTake args
handleVerb (Verb "put") args = handlePut args
handleVerb (Verb "dig") args = handleDig args
handleVerb (Verb "use") args = handleUse args
handleVerb (Verb "inventory") args = handleInventory args
handleVerb (Verb "help") args = handleHelp args
handleVerb (Verb "open") args = handleOpen args
handleVerb v _ = throwError $ UnrecognizedVerb v

handleWalk
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleWalk [] = throwError $ MissingParameter "missing destination"
handleWalk args = do
  gameState <- lift get
  let world'     = gameState ^. world
      exitName   = keyArg args

  (exitId, exit) <- exitCurrentRoom exitName
  _ <- maybeThrow (RoomDoesNotExist (_exitFrom exit)) $
    M.lookup (_exitFrom exit) (world' ^. rooms)
  _ <- maybeThrow (RoomDoesNotExist (_exitTo exit)) $
    M.lookup (_exitTo exit) (world' ^. rooms)
  case _exitLock exit of
    Nothing -> movePlayer exit (world' ^. playerRoom)
    Just (Lock _ errText _ Locked) -> do
      addPlayerMessage errText
      emitEvent $ PlayerMoveFailed (world' ^. playerRoom) exitId
    Just (Lock _ _ _ Unlocked) -> movePlayer exit (world' ^. playerRoom)
  where
    movePlayer :: (MonadState GameState m, Monad m)
      => Exit
      -> EntityId Room
      -> ExceptT GameError m ()
    movePlayer ext rm
      | _exitFrom ext == rm = do
          gameState <- lift get
          let w' = (gameState ^. world) & playerRoom .~ _exitTo ext
          lift . put $ gameState & world .~ w'
          emitEvent $ PlayerMoved (_exitFrom ext) (_exitTo ext)
      | otherwise           = throwError SpaceWizard
    unlockDoor
      :: (MonadState GameState m, Monad m)
      => EntityId Exit
      -> ExceptT GameError m ()
    unlockDoor eid = do
      gameState <- lift get
      w' <- modifyExit (gameState ^. world) eid unlockExit
      lift . put $ gameState & world .~ w'
    unlockExit :: Exit -> Exit
    unlockExit exit@(Exit _ _ _ _ (Just lck)) =
      exit { _exitLock = Just lck { _lockState = Unlocked } }
    unlockExit exit = exit
    playerHasKeyItems :: [EntityId GameObject] -> [EntityId GameObject] -> Bool
    playerHasKeyItems playerInv exitKeyItems =
      let exitEntityIds = map getEntityId exitKeyItems
      in exitEntityIds `intersect` map getEntityId playerInv == exitEntityIds

handlePickup
  :: ( Monad m, MonadState GameState m )
  => [Text]
  -> ExceptT GameError m ()
handlePickup [] = throwError $ MissingParameter "pickup what?"
handlePickup args = do
  gameState <- lift get
  let objectName = keyArg args
      world'     = gameState ^. world

  -- Check if already in inventory
  case lookupTextCI objectName (_worldPlayerInventory world') of
    Just _ -> throwError $ InvalidCommand "You already have that!"
    Nothing -> pure ()

  objectId <- getObjectInCurrentRoom world' objectName
  _ <- maybeThrow SpaceWizard $
    M.lookup (world' ^. playerRoom) (world' ^. rooms)
  object <- maybeThrow (ObjectDoesNotExist objectId) $
    M.lookup objectId (world' ^. objects)
  itemVerbs <- maybeThrow SpaceWizard . itemObjectVerbAliasMap $ object
  let actualName = _gameObjectName object  -- Use actual object name from data
      w' = world'
        & rooms .~ M.adjust (removeItem actualName) (world' ^. playerRoom) (world' ^. rooms)
        & playerInventory .~ putObjectInInventory actualName objectId (world' ^. playerInventory)
        & playerVerbs .~ M.union (_worldPlayerVerbs world') itemVerbs
  lift . put $ gameState & world .~ w'
  addPlayerMessage $ "You pick up the " <> actualName <> "."
  emitEvent (ItemPickedUp objectId (world' ^. playerRoom))
  where
    removeItem objName r = r
      { _roomObjects = M.update (const Nothing) objName (_roomObjects r)
      }

handleLook
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleLook [] = do
  gameState <- lift get
  emitEvent $ PlayerLookedAtRoom (_worldPlayerRoom . _gameStateWorld $ gameState)
handleLook args =
  case peek "in" args of
    Just args' -> lookInContainer . keyArg $ args'
    Nothing -> do
      let targetName = keyArg args
      gameState <- lift get
      let world' = gameState ^. world
      -- Try as object first, then as exit
      maybeObject <- lift $ runExceptT $ getObjectInCurrentRoom world' targetName
      case maybeObject of
        Right objectId -> handleExamine args  -- Found as object, examine it
        Left _ -> do  -- Not an object, try as exit
          (exitId, exit) <- exitCurrentRoom targetName
          addPlayerMessage . _exitDescription $ exit
          emitEvent $ PlayerLookedAtExit exitId

lookInContainer
  :: ( MonadState GameState m, Monad m )
  => Text -> ExceptT GameError m ()
lookInContainer containerName = do
  gameState <- lift get
  let w = gameState ^. world
  containerId <- getObjectInCurrentRoom w containerName
  container'  <- getObject w containerId
  case _gameObjectObject container' of
    ObjectItem _ -> throwError $ InvalidObjectParameter containerName
    ObjectContainer cont ->
      case _containerLock cont of
        Nothing -> doLookInContainer containerId cont
        Just (ContainerLock lck _) ->
          case _lockState lck of
            Locked -> do
              let lockMsg = _lockFailMsg lck
              addPlayerMessage lockMsg
              emitEvent $ PlayerFailedToLookInContainer containerId
            Unlocked -> doLookInContainer containerId cont
  where
    doLookInContainer
      :: ( MonadState GameState m, Monad m)
      => EntityId GameObject
      -> Container
      -> ExceptT GameError m ()
    doLookInContainer contId cont = do
      gameState <- lift get
      items <- traverse (getObject $ gameState ^. world) $ M.elems (_containerItems cont)
      let itemNames = T.intercalate ", " . map _gameObjectName $ items
      addPlayerMessage $ "Inside the " <> containerName <> " you see: " <> itemNames
      emitEvent $ PlayerLookedInContainer contId

handleDrop
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleDrop [] = throwError $ MissingParameter "drop what?"
handleDrop args = do
  gameState <- lift get
  let world'      = gameState ^. world
      objectName  = keyArg args

  (objectId, object) <- getObjectByName objectName

  let objectVerbs = fromMaybe M.empty . itemObjectVerbAliasMap $ object
      actualName = _gameObjectName object  -- Use actual object name from data

  let w' = world'
        & rooms .~ M.adjust (addObject actualName objectId) (world' ^. playerRoom) (world' ^. rooms)
        & playerInventory .~ M.delete actualName (world' ^. playerInventory)
        & playerVerbs .~ M.difference (world' ^. playerVerbs) objectVerbs
  lift . put $ gameState & world .~ w'
  emitEvent $ ItemDropped objectId (world' ^. playerRoom)
  where
    addObject objName objId r
      = r { _roomObjects = M.insert objName objId (_roomObjects r) }

handleExamine
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleExamine [] = throwError $ MissingParameter "examine what?"
handleExamine args =
  let objectName = maybe (keyArg args) keyArg $ peek "my" args
  in case peek "my" args of
    Just _ ->
      examineInInventory objectName
    Nothing    ->
      examineInRoom objectName

examineInInventory
  :: ( MonadState GameState m, Monad m)
  => ItemName
  -> ExceptT GameError m ()
examineInInventory objectName = do
  gameState <- lift get
  let world' = gameState ^. world
  objectId <- getObjectInInventory world' objectName
  object <- getObject world' objectId

  addPlayerMessage . describeGameObject $ object
  emitEvent $ ItemExamined objectId

examineInRoom
  :: ( MonadState GameState m, Monad m )
  => ItemName
  -> ExceptT GameError m ()
examineInRoom objectName = do
  gameState <- lift get
  let world' = gameState ^. world
  objectId <- getObjectInCurrentRoom world' objectName
  object   <- getObject world' objectId

  addPlayerMessage . describeGameObject $ object
  emitEvent $ ItemExamined objectId

handleTake
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleTake args = do
  gameState <- lift get
  let world' = gameState ^. world
  case splitAtWord "from" args of
    Nothing       -> handlePickup args  -- Act as alias for pickup if no "from"
    Just (xs, ys) -> do
      let containerName = keyArg ys
          itemName      = keyArg xs

      objectId <- getObjectInCurrentRoom world' containerName
      object   <- getObject world' objectId
      case _gameObjectObject object of
        ObjectItem _ -> throwError SpaceWizard
        ObjectContainer container' -> do
          itemId <- maybeThrow (ObjectNotInContainer itemName) $
            lookupTextCI itemName (_containerItems container')
          let object' = object { _gameObjectObject = ObjectContainer container' { _containerItems = M.delete itemName (_containerItems container') } }

          let w' = world'
                & objects .~ M.insert objectId object' (world' ^. objects)
                & playerInventory .~ putObjectInInventory itemName itemId (world' ^. playerInventory)
          lift . put $ gameState & world .~ w'
          emitEvent $ ItemTakenFromContainer itemId objectId

handlePut
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handlePut args = do
  gameState <- lift get
  let world' = gameState ^. world
  case splitAtWord "in" args of
    Nothing -> throwError $ MissingParameter "put what in what?"
    Just (itemArgs, containerArgs) -> do
      let itemName = keyArg itemArgs
          containerName = keyArg containerArgs

      -- Get item from inventory
      itemId <- getObjectInInventory world' itemName
      item <- getObject world' itemId

      -- Get container from room
      containerId <- getObjectInCurrentRoom world' containerName
      containerObj <- getObject world' containerId

      -- Validate it's a container
      case _gameObjectObject containerObj of
        ObjectItem _ ->
          throwError $ InvalidObjectParameter "You can't put things in that."

        ObjectContainer cont -> do
          -- Check if locked
          case _containerLock cont of
            Just (ContainerLock lck _) | _lockState lck == Locked ->
              throwError $ InvalidObjectParameter $ "The " <> containerName <> " is locked!"
            _ -> do
              -- Check capacity
              let currentItems = M.size (_containerItems cont)
              when (currentItems >= _containerSize cont) $
                throwError $ InvalidObjectParameter $ "The " <> containerName <> " is full!"

              -- Check item size (validate it's an item, not a container)
              case _gameObjectObject item of
                ObjectContainer _ ->
                  throwError $ InvalidObjectParameter "You can't put a container in a container!"
                ObjectItem itemData -> do
                  when (_itemSize' itemData > _containerSize cont) $
                    throwError $ InvalidObjectParameter $
                      _gameObjectName item <> " is too large for " <> containerName <> "!"

                  -- Update container (add item)
                  let updatedContainer = containerObj
                        { _gameObjectObject = ObjectContainer $ cont
                            { _containerItems = M.insert itemName itemId (_containerItems cont)
                            }
                        }

                  -- Update world state
                  let w' = world'
                        & objects .~ M.insert containerId updatedContainer (world' ^. objects)
                        & playerInventory .~ M.delete itemName (world' ^. playerInventory)

                  lift . put $ gameState & world .~ w'
                  emitEvent $ ItemPutInContainer itemId containerId

handleDig
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleDig _ = do
  gameState <- lift get
  let world' = gameState ^. world
  let roomId = world' ^. playerRoom
  room <- currentRoom world'
  case _roomDig room of
    Left failMsg -> addPlayerMessage failMsg
    Right [] -> do
      addPlayerMessage "You dig but nothing seems to come of it..."
      emitEvent $ Dug roomId Nothing
    Right (DigDefinition successMsg mItem:rest) -> do
      updateCurrentRoom $ \r -> r { _roomDig = rest <$ _roomDig r }
      case mItem of
        Nothing -> do
          addPlayerMessage successMsg
          emitEvent $ Dug roomId Nothing
        Just itemId -> do
          object <- getObject world' itemId
          let objectName = _gameObjectName object
          -- Place item in current room, not directly in inventory
          updateCurrentRoom $ \r -> r { _roomObjects = M.insert objectName itemId (_roomObjects r) }
          addPlayerMessage successMsg
          emitEvent $ Dug roomId (Just itemId)

data Command = Unlock deriving (Eq, Show)

data ObjectInteraction
  = ObjectInteraction
  { objectInteractionSubject   :: EntityId GameObject
  , objectInteractionPredicate :: EntityId GameObject
  , objectInteractionCommand   :: Verb
  }
  deriving (Eq, Show)

evalObjectInteraction
  :: ( MonadState GameState m, Monad m )
  => ObjectInteraction
  -> ExceptT GameError m ()
evalObjectInteraction (ObjectInteraction subjectId predicateId command) = do
  gameState <- lift get
  let world' = gameState ^. world
  subjectObject <- getObject world' subjectId
  predicateObject <- getObject world' predicateId
  doObjectCommand subjectObject predicateObject command
  where
    doObjectCommand
      :: ( MonadState GameState m, Monad m )
      => GameObject
      -> GameObject
      -> Verb
      -> ExceptT GameError m ()
    doObjectCommand subjObj predObj (Verb "use") = do
      gameState <- lift get
      let world' = gameState ^. world
      case _gameObjectObject predObj of
        ObjectItem _ ->
          throwError
          $ InvalidObjectInteraction
          (_gameObjectName subjObj)
          (_gameObjectName predObj)
          Unlock
        ObjectContainer c ->
          case _containerLock c of
            Nothing -> throwError $ InvalidCommand (_gameObjectName predObj <> " is not locked!")
            Just (ContainerLock lck _) | subjectId `elem` _lockKeyItems lck -> do
                       let world'' = world'
                             & objects .~ M.adjust (unlock lck) predicateId (world' ^. objects)
                             & playerMessages .~ _lockSuccessMsg lck : world' ^. playerMessages
                       lift . put $ gameState & world .~ world''
                       emitEvent $ ContainerUnlocked predicateId subjectId
            Just (ContainerLock lck _) -> do
              let unlockError = _lockFailMsg lck
              addPlayerMessage unlockError
              emitEvent $ ContainerUnlockFailed predicateId subjectId
    doObjectCommand _ _ _ = throwError $ InvalidCommand ""

    unlock :: Lock -> GameObject -> GameObject
    unlock l@(Lock _ _ _ s) o@(GameObject _ _ (ObjectContainer c)) =
      let newLockState = l { _lockState = flipLockState s }
      in o { _gameObjectObject = ObjectContainer c { _containerLock = (\cl -> cl { _containerLockLock = newLockState }) <$> _containerLock c } }
    unlock _ o = o

    flipLockState :: LockState -> LockState
    flipLockState Locked = Unlocked
    flipLockState Unlocked = Locked

handleUse
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleUse args = do
  gameState <- lift get
  let world' = gameState ^. world
  case parseArgs args of
    Just (objectName, predicateName) -> do
      objectId <- getObjectInInventory world' objectName
      -- Try to find as exit first (for doors), then as object
      maybeExit <- lift $ runExceptT $ exitCurrentRoom predicateName
      case maybeExit of
        Right (exitId, exit) -> useItemOnExit objectId exitId exit
        Left _ -> do
          predId <- getObjectInCurrentRoom world' predicateName
          evalObjectInteraction $ ObjectInteraction objectId predId (Verb "use")
    Nothing -> addPlayerMessage "I don't know how to do that..."
  where
    parseArgs :: [Text] -> Maybe (Text, Text)
    parseArgs [] = Nothing
    parseArgs args' =
      case break (== "on") args' of
        ([], _) -> Nothing
        (_, []) -> Nothing
        result  -> do
          let (objectName, predicateName) = tupMap T.unwords (T.unwords . drop 1) result
          if T.null objectName || T.null predicateName
            then Nothing
            else Just (objectName, predicateName)

    tupMap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
    tupMap f g (x, y) = (f x, g y)

    useItemOnExit :: (MonadState GameState m, Monad m)
      => EntityId GameObject
      -> EntityId Exit
      -> Exit
      -> ExceptT GameError m ()
    useItemOnExit itemId exitId exit = do
      case _exitLock exit of
        Nothing -> addPlayerMessage "That doesn't need unlocking."
        Just (Lock keyItems _ successMsg Locked) ->
          if itemId `elem` keyItems
          then do
            addPlayerMessage successMsg
            -- Unlock the door - get fresh state after message was added
            gameState <- lift get
            let world' = gameState ^. world
            w' <- modifyExit world' exitId $ \e ->
              e { _exitLock = Just (Lock keyItems "" successMsg Unlocked) }
            lift . put $ gameState & world .~ w'
            emitEvent $ DoorUnlocked exitId
          else addPlayerMessage "That key doesn't fit this lock."
        Just (Lock _ _ _ Unlocked) ->
          addPlayerMessage "That's already unlocked."

handleInventory
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleInventory _ = do
  gameState <- lift get
  let world' = gameState ^. world
      inventory = _worldPlayerInventory world'
  if M.null inventory
    then addPlayerMessage "Your inventory is empty."
    else do
      objects <- getInventoryObjects world'
      let itemNames = fmap _gameObjectName objects
          inventoryText = "You are carrying:\n" <> T.intercalate "\n" (fmap ("  - " <>) itemNames)
      addPlayerMessage inventoryText

handleHelp
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleHelp _ = do
  let helpText = T.unlines
        [ "Available commands:"
        , "  look [exit/object]  - Describe current room or examine something"
        , "  inventory           - Show items you're carrying"
        , "  pickup <item>       - Pick up an item"
        , "  drop <item>         - Drop an item from inventory"
        , "  take <item>         - Take an item (alias for pickup)"
        , "  walk <exit>         - Go through an exit"
        , "  examine <object>    - Look at an object closely"
        , "  use <item> on <target> - Use an item on something"
        , "  open <container>    - Hint: use 'use key on container' or 'look in container'"
        , "  dig                 - Dig in the current room"
        , "  put <item> in <container> - Store item in container"
        , ""
        , "Special commands:"
        , "  $save <filename>    - Save your game"
        , "  $load <filename>    - Load a saved game"
        , "  $quit               - Exit the game"
        ]
  addPlayerMessage helpText

handleOpen
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleOpen [] = addPlayerMessage "Open what? Try: 'use key on <container>' to unlock, or 'look in <container>' to see contents."
handleOpen args = do
  let containerName = keyArg args
  addPlayerMessage $ "To open " <> containerName <> ", try:\n  'use <key> on " <> containerName <> "' to unlock it\n  'look in " <> containerName <> "' to see what's inside"

render :: Monad m => World -> ExceptT GameError m Scene
render world' = do
  room       <- currentRoom world'
  objects'   <- traverse (getObject world') $ M.elems (_roomObjects room)
  exits'     <- traverse getExit $ M.elems . _roomExits $ room
  pure $ Scene
    { _sceneTitle = _roomName room
    , _sceneDescription = _roomDescription room
    , _sceneObjects = objects'
    , _sceneExits = exits'
    , _sceneMessages = world' ^. playerMessages
    }
  where
    getExit exitId =
      case M.lookup exitId (world' ^. exits) of
        Nothing   -> throwError $ ExitDoesNotExist exitId
        Just exit -> pure exit

-- Utilities

-- | if the first word matches the head of the list return the rest of
-- the list otherwise nothing.
peek :: Text -> [Text] -> Maybe [Text]
peek _ [] = Nothing
peek word (word':words')
  | word == word' = Just words'
  | otherwise     = Nothing

-- | If we find the matching word in the input text and more than one
-- word on each side of it, return the pair of lists otherwise
-- nothing.
splitAtWord :: Text -> [Text] -> Maybe ([Text], [Text])
splitAtWord w ws = case span (/= w) ws of
  ([], _) -> Nothing
  (xs, ys) | length ys > 1 -> Just (xs, tail ys)
           | otherwise     -> Nothing

keyArg :: [Text] -> Text
keyArg = T.unwords  -- Don't convert to lowercase - preserve original case

maybeElem :: (Foldable t, Eq a) => a -> t a -> Maybe a
maybeElem x xs =
  if x `elem` xs
  then Just x
  else Nothing

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

maybeThrow :: Monad m => b -> Maybe a -> ExceptT b m a
maybeThrow _ (Just x) = pure x
maybeThrow y Nothing  = throwError y

maybeError :: String -> Maybe a -> a
maybeError _ (Just x)  = x
maybeError err Nothing = error err

eitherToInput :: MonadError e (InputT m) => Either e a -> InputT m a
eitherToInput (Left err)     = throwError err
eitherToInput (Right result) = pure result

updateGame :: GameState -> GameState
updateGame gameState =
  case runIdentity . (`runStateT` gameState) . runExceptT $ handle' of
    (Left err, g) -> updateGameErrors g err
    (Right _, gameState') ->
      let world' = gameState' ^. world
      in case runExcept $ render world' of
           Left renderErr -> updateGameErrors gameState' renderErr
           Right rendered ->
             gameState'
               & scenes .~ rendered : gameState' ^. scenes
               & world .~ world' { _worldPlayerMessages = [] }
               & inputBuffer .~ ""
               & gameErrors .~ []
  where
    updateGameErrors :: GameState -> GameError -> GameState
    updateGameErrors g e = g & gameErrors .~ [gameErrorText e]  -- Only show current error

data UserCommandResult
  = Quit
  | SaveGame FilePath
  | LoadGame FilePath
  | Update
  deriving (Eq, Show)

handleUserCommand :: GameState -> UserCommandResult
handleUserCommand state' =
  let inputTxt = state' ^. inputBuffer
      input = T.split isSpace inputTxt
  in case input of
    ("$quit":_) -> Quit
    ("$save":fname:_) -> SaveGame . T.unpack $ fname
    ("$load":fname:_) -> LoadGame . T.unpack $ fname
    _       -> Update

class Monad m => MonadReadFile m where
  readAFile :: FilePath -> m Text

instance MonadReadFile IO where
  readAFile = T.readFile

type StubMonadReadFile = [(FilePath, Text)]

instance Monad m => MonadReadFile (StateT StubMonadReadFile m) where
  readAFile fpath = do
    stubFiles <- get
    case lookup fpath stubFiles of
      Nothing  -> error "No such file"
      Just txt -> pure txt

class Monad m => MonadWriteFile m where
  writeAFile :: FilePath -> Text -> m ()

instance MonadWriteFile IO where
  writeAFile = T.writeFile

instance MonadIO m => MonadWriteFile (WriterT [(FilePath, Text)] m) where
  writeAFile fpath txt = tell [(fpath, txt)]

-- Special Commands

getSaveDir :: IO FilePath
getSaveDir = do
  homeDir <- getHomeDirectory
  pure $ homeDir </> ".adventure-engine" </> "saves"

saveGameStateIO :: FilePath -> GameState -> IO (Either GameError (FilePath, Text))
saveGameStateIO fileName gameState = do
  saveDir <- getSaveDir
  let fullPath = saveDir </> fileName
      saveFileContents = T.decodeUtf8 . LBS.toStrict . JSON.encode $ gameState
  T.writeFile fullPath saveFileContents
  pure $ Right (fullPath, saveFileContents)

loadGameStateIO :: FilePath -> IO (Either GameError GameState)
loadGameStateIO fileName = runExceptT $ do
  saveDir <- liftIO getSaveDir
  let fullPath = saveDir </> fileName
  rawContents <- liftIO $ T.readFile fullPath
  case JSON.eitherDecode' . LBS.fromStrict . T.encodeUtf8 $ rawContents of
    Left decodeError -> throwError . SaveLoadError . T.pack $ decodeError
    Right gameState -> pure gameState

-- MonadWriteFile/MonadReadFile versions for GUI (original)
saveGameState :: MonadWriteFile m => FilePath -> GameState -> ExceptT GameError m (FilePath, Text)
saveGameState fpath gameState = do
  let saveFileContents = T.decodeUtf8 . LBS.toStrict . JSON.encode $ gameState
  lift . writeAFile fpath $ saveFileContents
  pure (fpath, saveFileContents)

loadGameState :: MonadReadFile m => FilePath -> ExceptT GameError m GameState
loadGameState fpath = do
  rawContents <- lift . readAFile $ fpath
  case JSON.eitherDecode' . LBS.fromStrict . T.encodeUtf8 $ rawContents of
    Left decodeError -> throwError . SaveLoadError . T.pack $ decodeError
    Right gameState -> pure gameState

loadGameData :: IO GameState
loadGameData =
  withArchive ("data" </> "game.zip") $ do
    worldRaw <- getEntry =<< mkEntrySelector "world.json"
    eventRewardsRaw <- getEntry =<< mkEntrySelector "event_rewards.json"
    gameEndRewardsRaw <- getEntry =<< mkEntrySelector "game_end_rewards.json"
    entries <- getEntries
    imgMap <- forM (filter isImageEntry . M.keys $ entries) $ \entry -> do
      imgRaw <- getEntry entry
      let imgData = getImageData imgRaw
      pure (getEntryName entry, imgData)
    let world'
          = maybeError "Could not read world.json from game.zip"
          . JSON.decodeStrict'
          $ worldRaw
        eventRewards
          = maybeError "Could not read event_rewards.json from game.zip"
          . JSON.decodeStrict'
          $ eventRewardsRaw
        gameEndRewards'
          = maybeError "Could not read game_end_rewards.json from game.zip"
          . JSON.decodeStrict'
          $ gameEndRewardsRaw
    case initialGameState world' eventRewards gameEndRewards' of
      Left err -> throw err
      Right initialState -> pure initialState { _gameStateImageCache = M.fromList imgMap }
  where
    isImageEntry :: EntrySelector -> Bool
    isImageEntry = equalFilePath "images" . takeDirectory . unEntrySelector

getImageData :: ByteString -> ImageData
getImageData imgRaw =
  case Pic.decodeImage imgRaw of
    Left err -> error $ "loadGameData: error loading image:" ++ show err
    Right dimg ->
      let img = Pic.convertRGBA8 dimg
          imgW = Pic.imageWidth img
          imgH = Pic.imageHeight img
          imgData = vectorToByteString . Pic.imageData $ img
      in ImageData { _imageDataRaw = imgData
                   , _imageDataWidth = imgW
                   , _imageDataHeight = imgH
                   }

ensureDirectories :: IO ()
ensureDirectories = do
  userHomeRoot <- getHomeDirectory
  createDirectoryIfMissing False $ userHomeRoot </> ".adventure-engine"
  createDirectoryIfMissing False $ userHomeRoot </> ".adventure-engine" </> "saves"
