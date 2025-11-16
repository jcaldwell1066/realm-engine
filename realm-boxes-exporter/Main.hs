{-# LANGUAGE OverloadedStrings #-}

-- | Realm Boxes Exporter
-- Exports realm-engine world state to boxes-live compatible JSON format
module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as T
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.IO (stderr)

import Adventure.BoxesLive
import Adventure.Engine.Database.Loader (loadWorldState)
import Adventure.Engine.Database.Connection (connectDB, closeDB)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printHelp
    ["-h"] -> printHelp
    ["--version"] -> putStrLn "realm-boxes-exporter 0.1.0"
    [dbConnStr, outputFile] -> exportWorld dbConnStr outputFile
    [dbConnStr] -> exportWorld dbConnStr "-"  -- stdout
    [] -> die "Error: Missing arguments. Use --help for usage."
    _ -> die "Error: Too many arguments. Use --help for usage."

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ unlines
    [ "Usage: " ++ progName ++ " [OPTIONS] <db-connection-string> [output-file]"
    , ""
    , "Export realm-engine world state to boxes-live JSON format"
    , ""
    , "Arguments:"
    , "  <db-connection-string>  PostgreSQL connection string"
    , "                          (e.g., \"host=localhost dbname=adventure user=postgres\")"
    , "  [output-file]           Output JSON file (default: stdout, use '-' for stdout)"
    , ""
    , "Options:"
    , "  -h, --help             Show this help message"
    , "  --version              Show version information"
    , ""
    , "Examples:"
    , "  " ++ progName ++ " \"host=localhost dbname=adventure\" world.json"
    , "  " ++ progName ++ " \"host=localhost dbname=adventure\" - | jq ."
    , ""
    , "Output Format:"
    , "  The JSON output contains:"
    , "  - boxes: List of room boxes with position, size, and content"
    , "  - connections: List of connections between rooms (exits)"
    , "  - metadata: World metadata (player location, version, etc.)"
    , ""
    , "Integration:"
    , "  Use this output with boxes-live to visualize the game world:"
    , "  $ " ++ progName ++ " \"...\" world.json"
    , "  $ boxes-live --import world.json"
    ]

exportWorld :: String -> FilePath -> IO ()
exportWorld connStr outputPath = do
  T.hPutStrLn stderr "Connecting to database..."
  conn <- connectDB connStr

  T.hPutStrLn stderr "Loading world state..."
  world <- loadWorldState conn

  T.hPutStrLn stderr "Converting to boxes-live format..."
  let canvas = worldToCanvas world
      jsonOutput = exportCanvasJSON canvas

  T.hPutStrLn stderr $ "Exporting to: " <> if outputPath == "-" then "<stdout>" else outputPath
  if outputPath == "-"
    then LBS.putStr jsonOutput
    else LBS.writeFile outputPath jsonOutput

  closeDB conn
  T.hPutStrLn stderr "Export complete!"
