{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Ermine.Console.Command.Common
  ( Command(Command)
  , HasCommand(..)
  , cmd
  ) where

import Control.Lens
import Ermine.Console.State
import System.Console.Haskeline

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
