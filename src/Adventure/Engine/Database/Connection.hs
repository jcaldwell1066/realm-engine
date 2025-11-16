{-# LANGUAGE OverloadedStrings #-}

module Adventure.Engine.Database.Connection
  ( DbConfig(..)
  , defaultDbConfig
  , buildConnectionString
  , withConnection
  , testConnection
  ) where

import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Control.Exception (bracket)

-- | Database configuration
data DbConfig = DbConfig
  { dbHost     :: T.Text
  , dbPort     :: Int
  , dbName     :: T.Text
  , dbUser     :: T.Text
  , dbPassword :: T.Text
  } deriving (Show, Eq)

-- | Default configuration for local development
defaultDbConfig :: DbConfig
defaultDbConfig = DbConfig
  { dbHost     = "localhost"
  , dbPort     = 5432
  , dbName     = "adventure_engine"
  , dbUser     = "postgres"
  , dbPassword = ""
  }

-- | Build a PostgreSQL connection string from config
buildConnectionString :: DbConfig -> ConnectInfo
buildConnectionString config = ConnectInfo
  { connectHost     = T.unpack $ dbHost config
  , connectPort     = fromIntegral $ dbPort config
  , connectUser     = T.unpack $ dbUser config
  , connectPassword = T.unpack $ dbPassword config
  , connectDatabase = T.unpack $ dbName config
  }

-- | Execute an action with a database connection
-- Automatically closes the connection when done
withConnection :: DbConfig -> (Connection -> IO a) -> IO a
withConnection config action =
  bracket
    (connect $ buildConnectionString config)
    close
    action

-- | Test database connectivity
-- Returns True if connection succeeds, False otherwise
testConnection :: DbConfig -> IO Bool
testConnection config = do
  result <- withConnection config $ \conn -> do
    [Only n] <- query_ conn "SELECT 1 :: integer" :: IO [Only Int]
    return (n == 1)
  return result
