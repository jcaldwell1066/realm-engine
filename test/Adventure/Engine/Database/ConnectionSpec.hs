{-# LANGUAGE OverloadedStrings #-}

module Adventure.Engine.Database.ConnectionSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Adventure.Engine.Database.Connection
import Database.PostgreSQL.Simple (ConnectInfo(..))

spec :: Spec
spec = do
  describe "DbConfig" $ do
    it "has sensible defaults" $ do
      let config = defaultDbConfig
      dbHost config `shouldBe` "localhost"
      dbPort config `shouldBe` 5432
      dbName config `shouldBe` "adventure_engine"
      dbUser config `shouldBe` "postgres"

  describe "buildConnectionString" $ do
    it "converts DbConfig to ConnectInfo" $ do
      let config = DbConfig
            { dbHost = "testhost"
            , dbPort = 5433
            , dbName = "testdb"
            , dbUser = "testuser"
            , dbPassword = "testpass"
            }
          connInfo = buildConnectionString config

      connectHost connInfo `shouldBe` "testhost"
      connectPort connInfo `shouldBe` 5433
      connectDatabase connInfo `shouldBe` "testdb"
      connectUser connInfo `shouldBe` "testuser"
      connectPassword connInfo `shouldBe` "testpass"

    it "handles default configuration" $ do
      let connInfo = buildConnectionString defaultDbConfig

      connectHost connInfo `shouldBe` "localhost"
      connectPort connInfo `shouldBe` 5432
      connectDatabase connInfo `shouldBe` "adventure_engine"
      connectUser connInfo `shouldBe` "postgres"

  -- Note: testConnection requires a running PostgreSQL instance
  -- These tests are skipped in CI environments without database access
  describe "testConnection (integration tests)" $ do
    it "returns False for invalid connection" $
      pendingWith "Requires mock or test database setup"

    it "returns True for valid connection" $
      pendingWith "Requires test database setup"
