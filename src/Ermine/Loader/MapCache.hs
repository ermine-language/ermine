{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- A Loader wrapper that caches in a Map
--------------------------------------------------------------------

module Ermine.Loader.MapCache
  ( CacheLoader(..)
  , HasCacheLoader(..)
  , cacheLoad
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Typeable
import Ermine.Loader.Core
import GHC.Generics (Generic)

-- | A loader with an associated cache.  Various operations from
-- 'Loader' are still possible, like lifting 'b -> m c', 'm ~> n', and
-- various composition, but it is usually easier to do these before
-- dropping the 'Loader' in 'CacheLoader'.
data CacheLoader e m a b = CacheLoader
  { _cacheSource :: Loader e m a b
  , _cacheMap :: M.Map a (e, b) }
  deriving (Functor, Generic, Typeable)

makeClassy ''CacheLoader

-- | Ignoring cached values, load the value and replace the cache.
cacheFreshLoad :: (Ord a, Monad m, HasCacheLoader s e m a b)
                => a -> StateT s m b
cacheFreshLoad a = do
  CacheLoader l m <- use cacheLoader
  eb@(_, b) <- lift (l ^. load $ a)
  cacheLoader .= CacheLoader l (M.insert a eb m)
  return b

-- | Load from cache if possible, freshly otherwise.
cacheLoad :: (Ord a, Monad m, HasCacheLoader s e m a b)
           => a -> StateT s m b
cacheLoad a = do
  CacheLoader l m <- use cacheLoader
  (_, b) <- maybe (do eb' <- lift $ view load l a
                      cacheLoader .= CacheLoader l (M.insert a eb' m)
                      return eb')
                  (\eb@(e, _) -> liftM (fromMaybe eb) . runMaybeT $ do
                      eb' <- MaybeT . lift $ view reload l a e
                      cacheLoader .= CacheLoader l (M.insert a eb' m)
                      return eb')
                  (M.lookup a m)
  return b
