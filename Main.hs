{-# LANGUAGE TemplateHaskell #-}
module Main ( main ) where

import Control.Applicative
import Control.Exception.Lens
import Control.Lens
import Control.Monad.State.Strict
import Data.Char
import Data.Default
import Ermine.Console.State
import Ermine.Console.Command
import Ermine.Console.Completion
import Ermine.Version
import System.Console.Haskeline
import System.Exit.Lens

main :: IO ()
main = do
  evalStateT ?? def $ runInputT settings $ do
    outputStrLn logo
    loop

loop :: InputT Console ()
loop = do
  minput <- getInputLine ">> "
  case dropWhile isSpace <$> minput of
    Nothing      -> return ()
    Just "quit"  -> return ()
    Just (':':cmd) -> do
      s <- lift get
      mt <- liftIO $ handling (filtered (hasn't _ExitCode)) (\xs -> Nothing <$ print xs) $
                   Just <$> (execStateT ?? s $ executeCommand cmd)
      maybe (return ()) (lift . put) mt
      loop
    Just ""      -> loop
    Just input   -> do
      outputStrLn $ "Input was: " ++ input
      loop
