{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Catalog
  ( -- * Types
    Catalog(..)
  , RealmMetadata(..)
  , Category(..)
  , Difficulty(..)
    -- * Loading
  , loadCatalog
  , loadRealmMetadata
    -- * Querying
  , filterByCategory
  , filterByDifficulty
  , filterByTag
  , filterByFeature
  , searchRealms
  , getRealmById
    -- * Display
  , printRealmInfo
  , printCatalogSummary
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Category of realm
data Category
  = Tutorial
  | TeamBuilding
  | Adventure
  | Puzzle
  | Challenge
  deriving (Eq, Show, Generic)

instance FromJSON Category
instance ToJSON Category

-- | Difficulty level
data Difficulty
  = Beginner
  | Intermediate
  | Advanced
  deriving (Eq, Show, Generic)

instance FromJSON Difficulty
instance ToJSON Difficulty

-- | Metadata for a single realm
data RealmMetadata = RealmMetadata
  { id :: Text
  , name :: Text
  , description :: Text
  , category :: Text
  , difficulty :: Text
  , estimatedMinutes :: Int
  , rooms :: Int
  , objects :: Int
  , author :: Text
  , version :: Text
  , created :: Text
  , tags :: [Text]
  , features :: [Text]
  , learningObjectives :: [Text]
  , path :: Maybe Text
  , walkthrough :: Maybe [Text]
  } deriving (Eq, Show, Generic)

instance FromJSON RealmMetadata
instance ToJSON RealmMetadata

-- | Complete catalog of all realms
data Catalog = Catalog
  { version :: Text
  , description :: Text
  , realms :: [RealmMetadata]
  } deriving (Eq, Show, Generic)

instance FromJSON Catalog
instance ToJSON Catalog

-- | Load catalog from JSON file
loadCatalog :: FilePath -> IO (Either String Catalog)
loadCatalog path = eitherDecodeFileStrict path

-- | Load individual realm metadata
loadRealmMetadata :: FilePath -> IO (Either String RealmMetadata)
loadRealmMetadata path = eitherDecodeFileStrict path

-- | Filter realms by category
filterByCategory :: Text -> Catalog -> [RealmMetadata]
filterByCategory cat catalog = filter (\r -> category r == cat) (realms catalog)

-- | Filter realms by difficulty
filterByDifficulty :: Text -> Catalog -> [RealmMetadata]
filterByDifficulty diff catalog = filter (\r -> difficulty r == diff) (realms catalog)

-- | Filter realms by tag
filterByTag :: Text -> Catalog -> [RealmMetadata]
filterByTag tag catalog = filter (elem tag . tags) (realms catalog)

-- | Filter realms by feature
filterByFeature :: Text -> Catalog -> [RealmMetadata]
filterByFeature feature catalog = filter (elem feature . features) (realms catalog)

-- | Search realms by keyword (searches name and description)
searchRealms :: Text -> Catalog -> [RealmMetadata]
searchRealms keyword catalog = filter matches (realms catalog)
  where
    keyword' = T.toLower keyword
    matches r = T.isInfixOf keyword' (T.toLower $ name r)
             || T.isInfixOf keyword' (T.toLower $ description r)

-- | Get realm by ID
getRealmById :: Text -> Catalog -> Maybe RealmMetadata
getRealmById realmId catalog =
  case filter (\r -> Catalog.id r == realmId) (realms catalog) of
    [realm] -> Just realm
    _       -> Nothing

-- | Print formatted realm information
printRealmInfo :: RealmMetadata -> IO ()
printRealmInfo realm = do
  putStrLn $ "=== " ++ T.unpack (name realm) ++ " ==="
  putStrLn $ "ID: " ++ T.unpack (Catalog.id realm)
  putStrLn $ "Category: " ++ T.unpack (category realm)
  putStrLn $ "Difficulty: " ++ T.unpack (difficulty realm)
  putStrLn $ "Duration: ~" ++ show (estimatedMinutes realm) ++ " minutes"
  putStrLn $ "Rooms: " ++ show (rooms realm)
  putStrLn $ "Objects: " ++ show (objects realm)
  putStrLn $ "\nDescription:"
  putStrLn $ "  " ++ T.unpack (description realm)
  putStrLn $ "\nTags: " ++ T.unpack (T.intercalate ", " (tags realm))
  putStrLn $ "Features: " ++ T.unpack (T.intercalate ", " (features realm))
  putStrLn $ "\nLearning Objectives:"
  mapM_ (\obj -> putStrLn $ "  - " ++ T.unpack obj) (learningObjectives realm)
  putStrLn ""

-- | Print catalog summary
printCatalogSummary :: Catalog -> IO ()
printCatalogSummary catalog = do
  putStrLn $ "=== Realm Catalog v" ++ T.unpack (version catalog) ++ " ==="
  putStrLn $ T.unpack (description catalog)
  putStrLn $ "\nTotal Realms: " ++ show (length $ realms catalog)
  putStrLn "\nCategories:"
  let categories = countByField category (realms catalog)
  mapM_ (\(cat, count) -> putStrLn $ "  " ++ T.unpack cat ++ ": " ++ show count) categories
  putStrLn "\nDifficulty Levels:"
  let difficulties = countByField difficulty (realms catalog)
  mapM_ (\(diff, count) -> putStrLn $ "  " ++ T.unpack diff ++ ": " ++ show count) difficulties
  putStrLn ""

-- Helper function to count items by a field
countByField :: Eq a => (RealmMetadata -> a) -> [RealmMetadata] -> [(a, Int)]
countByField field rs = map (\x -> (x, count x)) unique
  where
    values = map field rs
    unique = foldr (\x acc -> if x `elem` acc then acc else x : acc) [] values
    count x = length $ filter (== x) values
