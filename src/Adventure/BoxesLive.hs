{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration module for exporting realm-engine world state to boxes-live format
-- boxes-live is a terminal-based interactive canvas (like Miro for terminals)
-- This module exports World/Room data as JSON in a format suitable for visualization
module Adventure.BoxesLive
  ( BoxCanvas(..)
  , Box(..)
  , Position(..)
  , BoxContent(..)
  , worldToCanvas
  , exportCanvasJSON
  , calculateRoomPositions
  ) where

import qualified Data.Aeson as JSON
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Adventure.Engine
import Adventure.Engine.Database
import Adventure.Engine.Objects

-- | Position in canvas coordinates (world space)
data Position = Position
  { posX :: Int
  , posY :: Int
  } deriving (Show, Eq, Generic)

instance JSON.ToJSON Position where
  toJSON (Position x y) = JSON.object
    [ "x" .= x
    , "y" .= y
    ]

instance JSON.FromJSON Position where
  parseJSON = JSON.withObject "Position" $ \v -> Position
    <$> v JSON..: "x"
    <*> v JSON..: "y"

-- | Content to display inside a box
data BoxContent = BoxContent
  { contentTitle :: Text
  , contentLines :: [Text]
  , contentTags :: [Text]
  } deriving (Show, Eq, Generic)

instance JSON.ToJSON BoxContent where
  toJSON (BoxContent title lines' tags) = JSON.object
    [ "title" .= title
    , "lines" .= lines'
    , "tags" .= tags
    ]

instance JSON.FromJSON BoxContent where
  parseJSON = JSON.withObject "BoxContent" $ \v -> BoxContent
    <$> v JSON..: "title"
    <*> v JSON..: "lines"
    <*> v JSON..: "tags"

-- | A box on the canvas (represents a room)
data Box = Box
  { boxId :: Text
  , boxPosition :: Position
  , boxWidth :: Int
  , boxHeight :: Int
  , boxContent :: BoxContent
  , boxIsPlayerLocation :: Bool
  } deriving (Show, Eq, Generic)

instance JSON.ToJSON Box where
  toJSON (Box id' pos width height content isPlayer) = JSON.object
    [ "id" .= id'
    , "position" .= pos
    , "width" .= width
    , "height" .= height
    , "content" .= content
    , "isPlayerLocation" .= isPlayer
    ]

instance JSON.FromJSON Box where
  parseJSON = JSON.withObject "Box" $ \v -> Box
    <$> v JSON..: "id"
    <*> v JSON..: "position"
    <*> v JSON..: "width"
    <*> v JSON..: "height"
    <*> v JSON..: "content"
    <*> v JSON..: "isPlayerLocation"

-- | Connection between boxes (represents exits)
data BoxConnection = BoxConnection
  { connFrom :: Text
  , connTo :: Text
  , connLabel :: Text
  } deriving (Show, Eq, Generic)

instance JSON.ToJSON BoxConnection where
  toJSON (BoxConnection from to label) = JSON.object
    [ "from" .= from
    , "to" .= to
    , "label" .= label
    ]

instance JSON.FromJSON BoxConnection where
  parseJSON = JSON.withObject "BoxConnection" $ \v -> BoxConnection
    <$> v JSON..: "from"
    <*> v JSON..: "to"
    <*> v JSON..: "label"

-- | The full canvas representation
data BoxCanvas = BoxCanvas
  { canvasBoxes :: [Box]
  , canvasConnections :: [BoxConnection]
  , canvasMetadata :: Map Text Text
  } deriving (Show, Eq, Generic)

instance JSON.ToJSON BoxCanvas where
  toJSON (BoxCanvas boxes conns meta) = JSON.object
    [ "boxes" .= boxes
    , "connections" .= conns
    , "metadata" .= meta
    ]

instance JSON.FromJSON BoxCanvas where
  parseJSON = JSON.withObject "BoxCanvas" $ \v -> BoxCanvas
    <$> v JSON..: "boxes"
    <*> v JSON..: "connections"
    <*> v JSON..: "metadata"

-- | Convert a Room to BoxContent
roomToBoxContent :: Room -> [GameObject] -> BoxContent
roomToBoxContent room objects =
  BoxContent
    { contentTitle = _roomName room
    , contentLines = [_roomDescription room] ++ objectLines
    , contentTags = ["room"] ++ if hasObjects then ["items"] else []
    }
  where
    hasObjects = not (null objects)
    objectLines = if hasObjects
                  then ["", "Items:"] ++ map (("  • " <>) . _gameObjectName) objects
                  else []

-- | Simple graph layout algorithm: grid-based positioning
-- For now, we'll use a simple strategy: arrange rooms in a grid
-- Later this could be enhanced with force-directed layout or manual positioning
calculateRoomPositions :: [EntityId Room] -> Map (EntityId Room) Position
calculateRoomPositions roomIds =
  M.fromList $ zip roomIds positions
  where
    numRooms = length roomIds
    cols = ceiling (sqrt (fromIntegral numRooms :: Double))
    positions = [Position (x * 60) (y * 20) |
                 (i, _) <- zip [0..] roomIds,
                 let x = i `mod` cols,
                 let y = i `div` cols]

-- | Convert World to BoxCanvas
worldToCanvas :: World -> BoxCanvas
worldToCanvas world =
  BoxCanvas
    { canvasBoxes = boxes
    , canvasConnections = connections
    , canvasMetadata = M.fromList
        [ ("type", "realm-engine-world")
        , ("version", "0.1.0")
        , ("playerRoom", entityIdToText $ _worldPlayerRoom world)
        ]
    }
  where
    rooms = M.toList $ _worldRooms world
    roomPositions = calculateRoomPositions (map fst rooms)

    boxes = flip map rooms $ \(roomId, room) ->
      let objects = getObjectsInRoom world room
          position = M.findWithDefault (Position 0 0) roomId roomPositions
          isPlayerHere = roomId == _worldPlayerRoom world
      in Box
           { boxId = entityIdToText roomId
           , boxPosition = position
           , boxWidth = 40
           , boxHeight = 10
           , boxContent = roomToBoxContent room objects
           , boxIsPlayerLocation = isPlayerHere
           }

    connections = concatMap (roomExitsToConnections world) rooms

-- | Helper: get all objects in a room
getObjectsInRoom :: World -> Room -> [GameObject]
getObjectsInRoom world room =
  mapMaybe lookupObject (M.elems $ _roomObjects room)
  where
    lookupObject objId = M.lookup objId (_worldObjects world)

-- | Convert room exits to box connections
roomExitsToConnections :: World -> (EntityId Room, Room) -> [BoxConnection]
roomExitsToConnections world (roomId, room) =
  mapMaybe (exitToConnection roomId) (M.elems $ _roomExits room)
  where
    exitToConnection fromId exitId =
      case M.lookup exitId (_worldExits world) of
        Nothing -> Nothing
        Just exit' ->
          Just $ BoxConnection
            { connFrom = entityIdToText fromId
            , connTo = entityIdToText (_exitTo exit')
            , connLabel = _exitName exit'
            }

-- | Helper to convert EntityId to Text
entityIdToText :: EntityId a -> Text
entityIdToText (EntityId i) = "room_" <> T.pack (show i)

-- | Export canvas to JSON ByteString
exportCanvasJSON :: BoxCanvas -> LBS.ByteString
exportCanvasJSON = JSON.encode
