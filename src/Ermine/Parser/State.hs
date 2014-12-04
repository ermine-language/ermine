{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  , ParserLoader(..)
  , HasParserLoader(..)
  , parserLoad
  , parserReload
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Data (Data)
import qualified Data.Map as M
import Data.Typeable
import Ermine.Loader.Core
import Ermine.Syntax.Module

data ParseState = ParseState
  { _stateFixities :: [FixityDecl] }
  deriving (Show, Eq, Data, Typeable)

makeClassy ''ParseState

data ParserLoader m a b =
  forall e. ParserLoader (Loader e m a b) (M.Map a e)

instance HasFixities ParseState where
  fixityDecls = stateFixities

-- | A parser at the beginning.
initialParserState :: ParseState
initialParserState = ParseState []

class HasParserLoader s m a b | s -> m a b where
  parserLoader :: Lens' s (ParserLoader m a b)

instance HasParserLoader (ParserLoader m a b) m a b where
  parserLoader = id

parserLoad :: (Ord a, Monad m, HasParserLoader s m a b)
            => a -> StateT s m b
parserLoad a = do
  ParserLoader l m <- use parserLoader
  (e, b) <- lift (l ^. load $ a)
  parserLoader .= ParserLoader l (M.insert a e m)
  return b

parserReload :: (Ord a, Monad m, HasParserLoader s m a b)
             => a -> StateT s m (Maybe b)
parserReload a = runMaybeT $ do
  ParserLoader l m <- use parserLoader
  (e, b) <- maybe (lift . lift $ view load l a)
                  (MaybeT . lift . view reload l a)
                  (M.lookup a m)
  parserLoader .= ParserLoader l (M.insert a e m)
  return b
