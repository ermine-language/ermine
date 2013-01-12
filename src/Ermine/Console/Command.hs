{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Ermine.Console.Command
  ( Command(Command)
  , HasCommand(..)
  , commands
  , executeCommand
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Ermine.Console.State
import System.Console.Haskeline hiding (display)
import System.Console.Terminfo.PrettyPrint
import System.Exit
import Text.PrettyPrint.Free

data Command = Command
  { _name   :: String
  , _alts   :: [String]
  , _arg    :: Maybe (String)
  , _tabbed :: Maybe (CompletionFunc Console)
  , _desc   :: String
  , _body   :: String -> Console ()
  }

makeClassy ''Command

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

cmd :: String -> Command
cmd nm = Command nm [] Nothing Nothing "" $ \_ -> return ()

showHelp :: String -> Console ()
showHelp _ = liftIO $ displayLn $ vsep (map format commands) where
  format c = fill 18 (withArg c) <+> hang 18 (fillSep (text <$> words (c^.desc)))
  withArg c = case c^.arg of
    Nothing -> bold (char ':' <> text (c^.name))
    Just a  -> bold (char ':' <> text (c^.name)) <+> angles (dim (text a))

commands :: [Command]
commands =
  [ cmd "help" & desc .~ "show help"
               & alts .~ ["?"]
               & body .~ showHelp
  , cmd "load" & arg  ?~ "filename"
               & desc .~ "load a file"
               & body .~ \xs -> liftIO $ putStrLn =<< readFile xs
  , cmd "quit" & desc .~ "quit"
               & body.mapped .~ liftIO exitSuccess
  , cmd "kind" & desc .~ "infer the kind of a type"
               & body .~ \_ -> liftIO (putStrLn "bzzt")
  ]
