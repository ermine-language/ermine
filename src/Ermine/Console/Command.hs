{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Ermine.Console.Command
  ( commands
  , executeCommand
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Ermine.Console.Command.Common
import Ermine.Console.Command.Kind (kindCommand)
import Ermine.Console.State
import System.Console.Terminfo.PrettyPrint
import System.Exit
import Text.PrettyPrint.Free

getCommand :: String -> Maybe (Command, String)
getCommand zs = commands ^?
    folded.
    filtered (\c -> isPrefixOf xs (c^.name) || anyOf (alts.folded) (isPrefixOf xs) c).
    to (,ys')
  where
    (xs, ys) = break isSpace zs
    ys' = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace ys

executeCommand :: String -> Console ()
executeCommand txt = case getCommand txt of
  Just (c,args)  -> view body c args
  Nothing          -> do
    liftIO $ displayLn $ ring AudibleBellPreferred <> text "ermine: error: Unknown command:" <+> text (show txt)
    showHelp txt

showHelp :: String -> Console ()
showHelp _ = liftIO $ displayLn $ vsep (map format commands) where
  format c = fill 18 (withArg c) <+> hang 18 (fillSep (text <$> words (c^.desc)))
  withArg c = case c^.arg of
    Nothing -> bold (char ':' <> text (c^.name))
    Just a  -> bold (char ':' <> text (c^.name)) <+> angles (dim (text a))

helpCommand :: Command
helpCommand = cmd "help"
  & desc .~ "show help"
  & alts .~ ["?"]
  & body .~ showHelp

quitCommand :: Command
quitCommand =  cmd "quit"
  & desc .~ "quit"
  & body.mapped .~ liftIO exitSuccess

commands :: [Command]
commands =
  [ helpCommand
  , kindCommand
  , quitCommand
  -- , cmd "load" & arg  ?~ "filename"
  --             & desc .~ "load a file"
  --             & body .~ \xs -> liftIO $ putStrLn =<< readFile xs
  ]
