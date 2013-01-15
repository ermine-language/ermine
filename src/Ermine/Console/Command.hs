{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Ermine.Console.Command
  ( Command(..)
  , HasCommand(..)
  , commands
  , executeCommand
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Semigroup
import Ermine.Console.State
import Ermine.Parser.Kind
import Ermine.Syntax.Kind
import System.Console.Haskeline
import System.Console.Terminfo.PrettyPrint
import System.Exit
import Text.Trifecta.Parser
import Text.PrettyPrint.Free

------------------------------------------------------------------------------
-- Command
------------------------------------------------------------------------------

data Command = Command
  { _name   :: String
  , _alts   :: [String]
  , _arg    :: Maybe (String)
  , _tabbed :: Maybe (CompletionFunc Console)
  , _desc   :: String
  , _body   :: String -> Console ()
  }

makeClassy ''Command

cmd :: String -> Command
cmd nm = Command nm [] Nothing Nothing "" $ \_ -> return ()

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
    displayLn $ ring AudibleBellPreferred <> text "ermine: error: Unknown command:" <+> text (show txt)
    showHelp txt

showHelp :: String -> Console ()
showHelp _ = displayLn $ vsep (map format commands) where
  format c = fill 18 (withArg c) <+> hang 18 (fillSep (text <$> words (c^.desc)))
  withArg c = case c^.arg of
    Nothing -> bold (char ':' <> text (c^.name))
    Just a  -> bold (char ':' <> text (c^.name)) <+> angles (dim (text a))

------------------------------------------------------------------------------
-- commands
------------------------------------------------------------------------------

parsing :: Parser a -> (a -> Console ()) -> String -> Console ()
parsing p k s = case parseString p mempty s of
  Success a   -> k a
  Failure doc -> displayLn doc

commands :: [Command]
commands =
  [ cmd "help" & desc .~ "show help" & alts .~ ["?"] & body .~ showHelp
  , cmd "quit" & desc .~ "quit" & body.mapped .~ liftIO exitSuccess
  , cmd "uglykind" & desc .~ "show the internal representation of a kind schema" & body .~ parsing kind (liftIO . print . general)
  -- , cmd "load" & arg  ?~ "filename" & desc .~ "load a file" & body .~ \xs -> liftIO $ putStrLn =<< readFile xs
  ]
