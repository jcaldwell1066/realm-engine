{-# LANGUAGE OverloadedStrings #-}

module Adventure.BoxesLiveSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Hspec

import Adventure.BoxesLive
import Adventure.Engine
import Adventure.Engine.Database (EntityId(..))
import Adventure.Engine.Objects

spec :: Spec
spec = do
  describe "BoxesLive Integration" $ do
    describe "worldToCanvas" $ do
      it "creates a canvas with boxes for each room" $ do
        let world = createTestWorld
        let canvas = worldToCanvas world
        length (canvasBoxes canvas) `shouldBe` 3

      it "marks the player's current location" $ do
        let world = createTestWorld
        let canvas = worldToCanvas world
        let playerBoxes = filter boxIsPlayerLocation (canvasBoxes canvas)
        length playerBoxes `shouldBe` 1
        boxId (head playerBoxes) `shouldBe` "room_1"

      it "creates connections for exits" $ do
        let world = createTestWorld
        let canvas = worldToCanvas world
        length (canvasConnections canvas) `shouldBe` 2  -- entrance->hall, hall->chamber

      it "includes metadata with player location" $ do
        let world = createTestWorld
        let canvas = worldToCanvas world
        M.lookup "playerRoom" (canvasMetadata canvas) `shouldBe` Just "room_1"

    describe "calculateRoomPositions" $ do
      it "generates unique positions for each room" $ do
        let roomIds = [EntityId 1, EntityId 2, EntityId 3]
        let positions = calculateRoomPositions roomIds
        M.size positions `shouldBe` 3
        length (M.elems positions) `shouldBe` 3

      it "positions rooms in a grid layout" $ do
        let roomIds = [EntityId 1, EntityId 2, EntityId 3, EntityId 4]
        let positions = calculateRoomPositions roomIds
        -- With 4 rooms, we expect a 2x2 grid
        -- Positions should be at (0,0), (60,0), (0,20), (60,20)
        all (\p -> posX p `mod` 60 == 0 && posY p `mod` 20 == 0) (M.elems positions)
          `shouldBe` True

-- Helper: Create a test world
createTestWorld :: World
createTestWorld =
  World
    { _worldRooms = M.fromList
        [ (EntityId 1, entrance)
        , (EntityId 2, hallway)
        , (EntityId 3, chamber)
        ]
    , _worldObjects = M.fromList
        [ (EntityId 10, key)
        , (EntityId 11, torch)
        ]
    , _worldExits = M.fromList
        [ (EntityId 20, exitEntranceToHall)
        , (EntityId 21, exitHallToChamber)
        ]
    , _worldPlayerRoom = EntityId 1
    , _worldPlayerInventory = M.empty
    , _worldPlayerVerbs = M.empty
    , _worldPlayerMessages = []
    }
  where
    entrance = Room
      { _roomName = "Entrance"
      , _roomDescription = "A grand entrance hall"
      , _roomObjects = M.fromList [("key", EntityId 10)]
      , _roomExits = M.fromList [("north", EntityId 20)]
      , _roomDig = Left "Nothing to dig here"
      , _roomBackground = Nothing
      }
    hallway = Room
      { _roomName = "Hallway"
      , _roomDescription = "A long corridor"
      , _roomObjects = M.fromList [("torch", EntityId 11)]
      , _roomExits = M.fromList [("north", EntityId 21)]
      , _roomDig = Left "Stone floor"
      , _roomBackground = Nothing
      }
    chamber = Room
      { _roomName = "Chamber"
      , _roomDescription = "The final chamber"
      , _roomObjects = M.empty
      , _roomExits = M.empty
      , _roomDig = Left "Solid rock"
      , _roomBackground = Nothing
      }
    key = GameObject
      { _gameObjectName = "ancient key"
      , _gameObjectDescription = "A rusty old key"
      , _gameObjectObject = ObjectItem $ Item 1 1 []
      }
    torch = GameObject
      { _gameObjectName = "torch"
      , _gameObjectDescription = "A flickering torch"
      , _gameObjectObject = ObjectItem $ Item 2 2 []
      }
    exitEntranceToHall = Exit
      { _exitName = "north door"
      , _exitDescription = "A wooden door"
      , _exitFrom = EntityId 1
      , _exitTo = EntityId 2
      , _exitLock = Nothing
      }
    exitHallToChamber = Exit
      { _exitName = "stone archway"
      , _exitDescription = "An ancient archway"
      , _exitFrom = EntityId 2
      , _exitTo = EntityId 3
      , _exitLock = Nothing
      }
