module Main where

import Ermine.Version
import System.Console.Haskeline

main :: IO ()
main = do
  runInputT defaultSettings { historyFile = Just ".ermine_history" } $ do
    outputStrLn logo
    loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine ">> "
  case minput of
    Nothing     -> return ()
    Just "quit" -> return ()
    Just ""     -> loop
    Just input -> do
      outputStrLn $ "Input was: " ++ input
      loop
