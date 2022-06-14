module Main where

import Prelude

import Data.Array (drop)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.Process (argv, exit)

run :: String -> Effect Unit
run source = do
  log source

runFile :: FilePath -> Effect Unit
runFile filePath = do
  source <- readTextFile Encoding.UTF8 filePath
  log $ "running file" <> filePath <> "..."
  run source


main :: Effect Unit
main = do
  commandLineArgs <- map (drop 2) argv
  case commandLineArgs of
    [] -> log "runPrompt"
    [filePath] -> runFile filePath
    _ -> do
      log "Usage: jlox [script]"
      exit 64
