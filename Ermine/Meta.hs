{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Meta
-- Copyright :  (c) Edward Kmett 2011-2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Meta variables for unification
--------------------------------------------------------------------
module Ermine.Meta
  ( Meta(..)
  -- * Our unification monad
  , U, evalU
  , newMeta
  , readMeta
  , writeMeta
  , zonkMeta
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.ST
import Data.STRef
import Data.Traversable

-- | @U@ provides a fresh variable supply for meta variables.
newtype U s a = U { unU :: StateT Int (ST s) a }
  deriving (Functor,Applicative,Monad,MonadState Int)

-- | Evaluate an expression in the @U@ monad with a fresh variable supply.
evalU :: (forall s. U s a) -> a
evalU s = runST (evalStateT (unU s) 0)

-- | Generate a fresh variable
fresh :: U s Int
fresh = id <+= 1

-- | Lift an @ST@ action into the @U@ monad.
st :: ST s a -> U s a
st = U . lift

-- | A meta variable for skolemization and unification
data Meta s f = Meta !Int (STRef s (Maybe (f (Meta s f)))) deriving Eq

-- | Construct a new meta variable
newMeta :: U s (Meta s f)
newMeta = Meta <$> fresh <*> st (newSTRef Nothing)
{-# INLINE newMeta #-}

-- | Read a meta variable
readMeta :: Meta s f -> U s (Maybe (f (Meta s f)))
readMeta (Meta _ v) = st $ readSTRef v
{-# INLINE readMeta #-}

-- | Write to a meta variable
writeMeta :: Meta s f -> f (Meta s f) -> U s ()
writeMeta (Meta _ v) a = st $ writeSTRef v (Just a)
{-# INLINE writeMeta #-}

-- | Expand meta variables recursively
zonkMeta :: (Monad f, Traversable f) => f (Meta s f) -> U s (f (Meta s f))
zonkMeta f = fmap join . for f $ \m -> readMeta m >>= \mv -> case mv of
    Nothing  -> return (return m)
    Just fmf -> do
      r <- zonkMeta fmf -- recursively expand
      writeMeta m r -- save the zonked version back to avoid work later
      return r
