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
    -- * Parser combinator integration
  , ErmineParserT(..)
  ) where

import Control.Applicative (Alternative, Applicative)
import Control.Lens
import Control.Monad.State
import Data.Typeable
import Ermine.Loader.MapCache
import Ermine.Syntax.Module
import Ermine.Syntax.ModuleName
import Text.Parser.Char (CharParsing)
import Text.Parser.Combinators (Parsing)
import Text.Parser.LookAhead (LookAheadParsing)
import Text.Parser.Token (TokenParsing)

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

-- | A monad transformer that adds a 'StateT' of a self-referential
-- 'ParseState' to its argument.
newtype ErmineParserT e m a =
  ErmineParserT { runErmineParserT ::
                     StateT (ParseState e (ErmineParserT e m)) m a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadIO,
            Parsing, CharParsing, LookAheadParsing, TokenParsing)

instance Monad m => MonadState (ParseState e (ErmineParserT e m))
                               (ErmineParserT e m) where
  get = ErmineParserT get
  put = ErmineParserT . put
  state = ErmineParserT . state

instance MonadTrans (ErmineParserT e) where
  lift = ErmineParserT . lift
