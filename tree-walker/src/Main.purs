module Main where

import Prelude

import Data.Array (drop)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.Process (argv, exit)
import Node.ReadLine as RL

runFile :: FilePath -> Effect Unit
runFile filePath = do
  source <- readTextFile Encoding.UTF8 filePath
  log $ "running file" <> filePath <> "..."
  log source

runPrompt :: Effect Unit
runPrompt = do
  interface <- RL.createConsoleInterface RL.noCompletion
  let lineHandler :: String -> Effect Unit
      lineHandler line = do
        log $ "got line: " <> line
        RL.prompt interface
  RL.setPrompt ">>> " interface
  RL.setLineHandler lineHandler interface
  RL.prompt interface


main :: Effect Unit
main = do
  commandLineArgs <- map (drop 2) argv
  case commandLineArgs of
    [] -> runPrompt
    [filePath] -> runFile filePath
    _ -> do
      log "Usage: jlox [script]"
      exit 64
