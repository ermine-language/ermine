{-# LANGUAGE TemplateHaskell #-}
module Main ( main ) where

import Control.Applicative
import Control.Exception.Lens
import Control.Lens
import Control.Monad.State.Strict
import Data.ByteString.Char8 as Char8
import Data.Char
import Data.Default
import Ermine.Console.State
import Ermine.Console.Command
import Ermine.Console.Completion
import Ermine.Console.Unicode
import Ermine.Version
import System.Console.Haskeline
import System.Exit.Lens
import System.Remote.Monitoring

main :: IO ()
main = withUnicode $ do
  server <- forkServer (Char8.pack "localhost") 5616
  evalStateT ?? def $ runInputT settings $ do
    outputStrLn logo
    loop server

loop :: Server -> InputT Console ()
loop server = do
  minput <- getInputLine ">> "
  case Prelude.dropWhile isSpace <$> minput of
    Nothing      -> return ()
    Just "quit"  -> return ()
    Just (':':cmd) -> do
      lift $ handling (filtered (hasn't _ExitCode)) (liftIO . print) (executeCommand cmd)
      loop server
    Just ""      -> loop server
    Just input   -> do
      outputStrLn $ "Input was: " ++ input
      loop server
