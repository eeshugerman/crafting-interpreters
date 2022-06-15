module Main where

import Prelude

import Data.Array (drop)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.Process (argv, exit)
import Node.ReadLine as RL

run :: String -> Effect Unit
run source = do
  log source

runFile :: FilePath -> Effect Unit
runFile filePath = do
  source <- readTextFile Encoding.UTF8 filePath
  log $ "running file" <> filePath <> "..."
  run source

runPrompt :: Effect Unit
runPrompt = do
  interface <- RL.createConsoleInterface RL.noCompletion
  loop interface

  where
    lineHandler :: String -> Effect Unit
    lineHandler line = do
      log $ "got line: " <> line

    loop :: RL.Interface -> Effect Unit
    loop interface = do
      RL.setPrompt ">>> " interface
      RL.setLineHandler lineHandler interface
      let
        callback :: Either Error Unit -> Effect Unit
        callback (Left err) = do
          log $ show err
        callback (Right _) = do
          loop interface

        aff :: Aff Unit
        aff = liftEffect $ RL.prompt interface
      runAff_ callback aff


main :: Effect Unit
main = do
  commandLineArgs <- map (drop 2) argv
  case commandLineArgs of
    [] -> runPrompt
    [filePath] -> runFile filePath
    _ -> do
      log "Usage: jlox [script]"
      exit 64
