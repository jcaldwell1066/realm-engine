module Main where

import Adventure.Engine
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Lens ((^.), (.~), (&))
import Control.Monad.Except (runExceptT)
import System.IO (hFlush, stdout)
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..))

-- Color helpers
setColor :: Color -> IO ()
setColor c = setSGR [SetColor Foreground Vivid c]

resetColor :: IO ()
resetColor = setSGR [Reset]

-- Simple CLI REPL for Adventure Engine
main :: IO ()
main = do
  putStrLn "🎮 Adventure Engine CLI"
  putStrLn "Loading game data..."
  gameState <- loadGameData
  putStrLn "Game loaded! Type 'help' for commands, '$quit' to exit.\n"

  -- Show initial room
  let initialScene = head (_gameStateScenes gameState)
  TIO.putStrLn $ _sceneDescription initialScene
  displayObjects initialScene
  displayExits initialScene
  putStrLn ""

  repl gameState

repl :: GameState -> IO ()
repl state = do
  setColor Cyan
  putStr "> "
  setColor Yellow  -- User input in yellow
  hFlush stdout
  input <- TIO.getLine
  resetColor

  let state' = state & inputBuffer .~ input
      result = handleUserCommand state'

  case result of
    Quit -> putStrLn "Goodbye!"
    SaveGame fname -> do
      saveResult <- saveGameStateIO fname state'
      case saveResult of
        Left err -> do
          TIO.putStrLn $ "Error saving game: " <> gameErrorText err
          repl state'
        Right (savedPath, _) -> do
          putStrLn $ "Game saved to: " ++ savedPath
          repl state'
    LoadGame fname -> do
      loadResult <- loadGameStateIO fname
      case loadResult of
        Left err -> do
          TIO.putStrLn $ "Error loading game: " <> gameErrorText err
          repl state'
        Right loadedState -> do
          putStrLn $ "Game loaded from: " ++ fname
          -- Show current room after loading
          case _gameStateScenes loadedState of
            (scene:_) -> do
              TIO.putStrLn $ _sceneDescription scene
              displayObjects scene
              displayExits scene
              putStrLn ""
            [] -> putStrLn ""
          repl loadedState
    Update -> do
      let newState = updateGame state'
      -- Print the latest scene
      case _gameStateScenes newState of
        (scene:_) -> do
          resetColor
          TIO.putStrLn $ _sceneDescription scene
          displayObjects scene
          displayExits scene
          -- Print player messages (dig results, examine, etc.)
          setColor Green
          mapM_ TIO.putStrLn (_sceneMessages scene)
          -- Print any errors
          setColor Red
          mapM_ TIO.putStrLn (_gameStateGameErrors newState)
          resetColor
          putStrLn ""
        [] -> putStrLn "Error: No scene to display\n"
      repl newState

displayObjects :: Scene -> IO ()
displayObjects scene =
  let objects = _sceneObjects scene
  in if null objects
     then pure ()
     else do
       putStrLn ""
       setColor Magenta
       TIO.putStrLn "You can see:"
       resetColor
       mapM_ (\obj -> TIO.putStrLn $ "  - " <> _gameObjectName obj) objects

displayExits :: Scene -> IO ()
displayExits scene =
  let exits = _sceneExits scene
  in if null exits
     then pure ()
     else do
       putStrLn ""
       setColor Blue
       TIO.putStrLn "Exits:"
       resetColor
       mapM_ (\exit -> TIO.putStrLn $ "  - " <> _exitName exit) exits
