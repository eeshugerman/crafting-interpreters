module Main where

import Prelude

import Data.Array (drop)
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, runAff_)
import Effect.Console (log, errorShow)
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
  runAff_
    -- (either
    --  (\err -> errorShow err *> RL.close interface)
    --  (const $ RL.close interface))
    \foo -> case foo of
      Left err -> errorShow err *> RL.close interface
    (loop interface)

  where
    loop interface = do
      RL.setPrompt ">>>" interface
      line <- RL.prompt interface
      log $ "got line: " <> line




main :: Effect Unit
main = do
  commandLineArgs <- map (drop 2) argv
  case commandLineArgs of
    [] -> runPrompt
    [filePath] -> runFile filePath
    _ -> do
      log "Usage: jlox [script]"
      exit 64
