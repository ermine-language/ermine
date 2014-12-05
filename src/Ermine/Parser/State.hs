{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) McGraw Hill Financial 2014
-- License   :  BSD2
-- Maintainer:  Stephen Compall <scompall@nocandysw.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- State for monadic parsing
--------------------------------------------------------------------

module Ermine.Parser.State
  ( ParseState(ParseState)
  , HasParseState(..)
  , initialParserState
  ) where

import Control.Lens
import Data.Data (Data)
import Data.Typeable
import Ermine.Syntax.Module

data ParseState = ParseState
  { _stateFixities :: [FixityDecl] }
  deriving (Show, Eq, Data, Typeable)

makeClassy ''ParseState

instance HasFixities ParseState where
  fixityDecls = stateFixities

-- | A parser at the beginning.
initialParserState :: ParseState
initialParserState = ParseState []
