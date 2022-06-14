module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Array (drop)
import Node.Process (argv, exit)

main :: Effect Unit
main = do
  commandLineArgs <- map (drop 2) argv
  case commandLineArgs of
    [] ->
      log "runPrompt"
    [filePath] ->
      log $ "runFile " <> filePath
    _ -> do
      log "Usage: jlox [script]"
      exit 64
