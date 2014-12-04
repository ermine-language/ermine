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
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Data (Data)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Typeable
import Ermine.Loader.Core
import Ermine.Syntax.Module

data ParseState = ParseState
  { _stateFixities :: [FixityDecl] }
  deriving (Show, Eq, Data, Typeable)

makeClassy ''ParseState

-- | A loader with an associated cache.  Various operations from
-- 'Loader' are still possible, like lifting 'b -> m c', 'm ~> n', and
-- various composition, but it is usually easier to do these before
-- dropping the 'Loader' in 'ParserLoader'.
data ParserLoader m a b =
  forall e. ParserLoader (Loader e m a b) (M.Map a (e, b))

instance HasFixities ParseState where
  fixityDecls = stateFixities

-- | A parser at the beginning.
initialParserState :: ParseState
initialParserState = ParseState []

class HasParserLoader s m a b | s -> m a b where
  parserLoader :: Lens' s (ParserLoader m a b)

instance HasParserLoader (ParserLoader m a b) m a b where
  parserLoader = id

-- | Ignoring cached values, load the value and replace the cache.
parserFreshLoad :: (Ord a, Monad m, HasParserLoader s m a b)
                => a -> StateT s m b
parserFreshLoad a = do
  ParserLoader l m <- use parserLoader
  eb@(_, b) <- lift (l ^. load $ a)
  parserLoader .= ParserLoader l (M.insert a eb m)
  return b

-- | Load from cache if possible, freshly otherwise.
parserLoad :: (Ord a, Monad m, HasParserLoader s m a b)
           => a -> StateT s m b
parserLoad a = do
  ParserLoader l m <- use parserLoader
  (_, b) <- maybe (do eb' <- lift $ view load l a
                      parserLoader .= ParserLoader l (M.insert a eb' m)
                      return eb')
                  (\eb@(e, _) -> liftM (fromMaybe eb) . runMaybeT $ do
                      eb' <- MaybeT . lift $ view reload l a e
                      parserLoader .= ParserLoader l (M.insert a eb' m)
                      return eb')
                  (M.lookup a m)
  return b
