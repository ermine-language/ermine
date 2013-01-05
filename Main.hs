module Main where

import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "% "
    case minput of
      Nothing     -> return ()
      Just "quit" -> return ()
      Just ""     -> loop
      Just input -> do
        outputStrLn $ "Input was: " ++ input
        loop
