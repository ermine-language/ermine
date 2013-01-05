module Main where

import Paths_ermine
import Control.Monad.Trans
import System.Console.Haskeline
import System.FilePath

main :: IO ()
main = runInputT defaultSettings { historyFile = Just ".ermine.history" } $ do
  logo
  loop

logo :: InputT IO ()
logo = do
  txt <- liftIO $ do Paths_ermine.getDataFileName ("data" </> "logo.txt") >>= readFile
  outputStrLn txt

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
