{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
  ( -- * Stateful parsing
    ParseState(ParseState)
  , HasParseState(..)
  , initialParserState
  ) where

import Control.Lens
import Data.Data
import Ermine.Syntax.Module
import Ermine.Syntax.ModuleName
import GHC.Generics (Generic)

data ParseState = ParseState
  { _stateFixities :: [FixityDecl]
  , _stateParsingModules :: [ModuleName] }
  deriving (Show, Eq, Data, Generic, Typeable)

makeClassy ''ParseState

instance HasFixities ParseState where
  fixityDecls = stateFixities

-- | A parser at the beginning.
initialParserState :: ParseState
initialParserState = ParseState [] []
