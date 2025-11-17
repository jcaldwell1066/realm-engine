module Main where

import Catalog
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []              -> showHelp
    ["list"]        -> listRealms
    ["show", rid]   -> showRealm (T.pack rid)
    ["category", c] -> listByCategory (T.pack c)
    ["search", q]   -> searchByKeyword (T.pack q)
    _               -> showHelp

showHelp :: IO ()
showHelp = do
  putStrLn "Realm Catalog Browser"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  catalog-browser list              - List all realms"
  putStrLn "  catalog-browser show <id>         - Show detailed info for a realm"
  putStrLn "  catalog-browser category <cat>    - Filter by category"
  putStrLn "  catalog-browser search <keyword>  - Search realms"
  putStrLn ""
  putStrLn "Categories: tutorial, team-building, adventure, puzzle, challenge"

listRealms :: IO ()
listRealms = do
  result <- loadCatalog "data/catalog.json"
  case result of
    Left err -> putStrLn $ "Error loading catalog: " ++ err
    Right catalog -> do
      printCatalogSummary catalog
      putStrLn "Available Realms:"
      putStrLn ""
      mapM_ printRealmSummary (realms catalog)

showRealm :: Text -> IO ()
showRealm realmId = do
  result <- loadCatalog "data/catalog.json"
  case result of
    Left err -> putStrLn $ "Error loading catalog: " ++ err
    Right catalog ->
      case getRealmById realmId catalog of
        Nothing -> putStrLn $ "Realm not found: " ++ T.unpack realmId
        Just realm -> printRealmInfo realm

listByCategory :: Text -> IO ()
listByCategory cat = do
  result <- loadCatalog "data/catalog.json"
  case result of
    Left err -> putStrLn $ "Error loading catalog: " ++ err
    Right catalog -> do
      let filtered = filterByCategory cat catalog
      putStrLn $ "Realms in category '" ++ T.unpack cat ++ "':"
      putStrLn ""
      mapM_ printRealmSummary filtered

searchByKeyword :: Text -> IO ()
searchByKeyword keyword = do
  result <- loadCatalog "data/catalog.json"
  case result of
    Left err -> putStrLn $ "Error loading catalog: " ++ err
    Right catalog -> do
      let results = searchRealms keyword catalog
      putStrLn $ "Search results for '" ++ T.unpack keyword ++ "':"
      putStrLn ""
      if null results
        then putStrLn "No realms found."
        else mapM_ printRealmSummary results

printRealmSummary :: RealmMetadata -> IO ()
printRealmSummary realm = do
  putStrLn $ "  [" ++ T.unpack (Catalog.id realm) ++ "] " ++ T.unpack (name realm)
  putStrLn $ "    " ++ T.unpack (category realm) ++ " | "
          ++ T.unpack (difficulty realm) ++ " | ~"
          ++ show (estimatedMinutes realm) ++ " min"
  putStrLn ""
