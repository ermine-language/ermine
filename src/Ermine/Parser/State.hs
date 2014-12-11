{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Typeable
import Ermine.Loader.MapCache
import Ermine.Syntax.Module
import Ermine.Syntax.ModuleName

data ParseState e m = ParseState
  { _stateFixities :: [FixityDecl]
  , _stateLoader :: CacheLoader e m ModuleName Module
  , _stateParsingModules :: [ModuleName] }
  deriving (Typeable)

makeClassy ''ParseState

instance Show (ParseState e m) where
  showsPrec i = showsPrec i . view stateFixities

instance HasFixities (ParseState e m) where
  fixityDecls = stateFixities

instance HasCacheLoader (ParseState e m) e m ModuleName Module where
  cacheLoader = stateLoader

-- | A parser at the beginning.
initialParserState :: CacheLoader e m ModuleName Module -> ParseState e m
initialParserState cl = ParseState [] cl []
