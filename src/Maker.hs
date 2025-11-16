module Maker where

import Codec.Archive.Zip
import qualified Data.Aeson as JSON
import qualified Data.Text.IO as T
import Maker.Parser
import Maker.SQL
import System.Directory
import System.Environment
import System.FilePath
import Text.Megaparsec

helpText :: String
helpText = "Usage: adventure-maker DIR"

run :: IO ()
run = do
  args <- getArgs
  case args of
    [path] -> processPath path
    _      -> putStrLn helpText

processPath :: FilePath -> IO ()
processPath path = do
  let worldTxtPath = path </> "world.txt"
      worldJsonPath = path </> "world.json"
      worldSqlPath = path </> "world.sql"
      outputPath = path </> "build"
  createDirectoryIfMissing True outputPath
  worldTxt <- T.readFile worldTxtPath
  let worldDataResult = runMakerParser worldParser worldTxtPath worldTxt
  case worldDataResult of
    Left err -> putStrLn . errorBundlePretty $ err
    Right worldData -> do
      -- Generate JSON output
      JSON.encodeFile worldJsonPath worldData
      -- Generate SQL output
      T.writeFile worldSqlPath $ generateWorldSQL worldData
      -- Create game.zip archive
      createArchive (outputPath </> "game.zip") $ do
        worldJsonEntry <- mkEntrySelector "world.json"
        loadEntry Deflate worldJsonEntry worldJsonPath
      -- Report success
      putStrLn $ "World compiled successfully!"
      putStrLn $ "  JSON: " ++ worldJsonPath
      putStrLn $ "  SQL:  " ++ worldSqlPath
      putStrLn $ "  ZIP:  " ++ (outputPath </> "game.zip")
