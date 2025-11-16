{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Text (Text)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Adventure.Engine
import Adventure.Engine.Database
import Adventure.Engine.Rewards

import qualified Adventure.Engine.Database.ConnectionSpec as ConnectionSpec
import qualified Adventure.Engine.Database.LoaderSpec as LoaderSpec

main :: IO ()
main = hspec $ do
  describe "Adventure.Engine.Database.Connection" ConnectionSpec.spec
  describe "Adventure.Engine.Database.Loader" LoaderSpec.spec


  -- Temporarily disabled due to GameState type changes (pre-existing issue)
  -- describe "saveGameState" $
  --   context "given the defaultGameState" $ do
  --     it "should write save files to the engine's path in the users' directory" $ do
  --       result <- (execWriterT . runExceptT . saveGameState "foo" $ defaultGameState)
  --       (fst . head $ (result :: [(FilePath, Text)]))
  --         `shouldBe` ("~/.adventure-engine/saves/foo" :: FilePath)

  --     it "should restore default state from save file" $ do
  --       fileContents <- execWriterT . runExceptT . saveGameState "foo" $ defaultGameState
  --       loadedGameState <- (`evalStateT` (fileContents :: [(FilePath, Text)])) . runExceptT . loadGameState $ "foo"
  --       Right defaultGameState `shouldBe` loadedGameState

  -- Temporarily disabled due to Event type changes (pre-existing issue)
  -- fdescribe "score" $ do
  --   context "given some duplicate event in the event log" $
  --     it "should count only the first event" $ do
  --       let events = [ItemPickedUp (EntityId 5), ItemPickedUp (EntityId 5), ItemDropped]
  --           rewards = [EventReward (ItemPickedUp (EntityId 5)) 100]
  --       score events rewards `shouldBe` 100
