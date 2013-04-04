{-# LANGUAGE TemplateHaskell #-}
module Main ( main ) where

import Control.Applicative
import Control.Exception.Lens
import Control.Lens
import Control.Monad.State.Strict
import Data.ByteString.Lens
import Data.Char
import Data.Default
import Data.Monoid
import Ermine
import Ermine.Console.State
import Ermine.Console.Command
import Ermine.Console.Completion
import Ermine.Console.Options
import Ermine.Console.Unicode
-- import Ermine.Version
import Options.Applicative
import System.Console.Haskeline
import System.Exit.Lens
import System.Remote.Monitoring

main :: IO ()
main = withUnicode $ do
  optionParser <- parseOptions

  opts <- execParser $ info (helper <*> optionParser) $
    fullDesc
    <> progDesc "The Ermine Programming Language"
    <> header ("Ermine " ++ version)

  putStrLn logo

  server <- if opts^.monitorEnabled
    then do
      putStrLn $ "Monitoring enabled at http://" ++ opts^.monitorHost ++ ":" ++ show (opts^.monitorPort) ++ "/"
      Just <$> forkServer (opts^.monitorHost.packedChars) (opts^.monitorPort)
    else pure Nothing

  evalStateT ?? def $ runInputT settings $ do
    loop server

loop :: Maybe Server -> InputT Console ()
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
