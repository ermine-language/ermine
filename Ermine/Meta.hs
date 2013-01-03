{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , semiprune
  -- , zonkMeta
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.ST
import Data.STRef
import Data.Traversable
import Ermine.Rendering
import Ermine.Variable

-- | @U@ provides a fresh variable supply for meta variables.
data Result a
  = Err Rendering String
  | OK !Int a

instance Functor Result where
  fmap f (OK n a) = OK n (f a)
  fmap _ (Err r s) = Err r s
  {-# INLINE fmap #-}

newtype U s a = U { unU :: Rendering -> Int -> ST s (Result a) }

instance Functor (U s) where
  fmap f (U m) = U $ \r s -> fmap f <$> m r s
  {-# INLINE fmap #-}

instance Applicative (U s) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (U s) where
  return a = U $ \_ n -> return (OK n a)
  {-# INLINE return #-}
  U m >>= k = U $ \ r n -> m r n >>= \s -> case s of
    OK n' a -> unU (k a) r n'
    Err t e -> return $ Err t e
  {-# INLINE (>>=) #-}
  fail s = U $ \r _ -> return (Err r s)

instance MonadReader Rendering (U s) where
  ask = U $ \r n -> return (OK n r)
  {-# INLINE ask #-}
  local f (U m) = U (m . f)
  {-# INLINE local #-}

-- | Evaluate an expression in the @U@ monad with a fresh variable supply.
evalU :: (forall s. U s a) -> Rendering -> Either (Rendering, String) a
evalU m r = case runST (unU m r 0) of
  Err t e -> Left (t, e)
  OK _ a  -> Right a
{-# INLINE evalU #-}

-- | Generate a fresh variable
fresh :: U s Int
fresh = U $ \ _ n -> return $ OK (n + 1) n
{-# INLINE fresh #-}

-- | Lift an @ST@ action into the @U@ monad.
st :: ST s a -> U s a
st s = U $ \_ n -> OK n <$> s
{-# INLINE st #-}

-- | A meta variable for skolemization and unification
data Meta s f = Meta !Int (STRef s (Maybe (f (Meta s f))))

instance Eq (Meta s f) where
  Meta n _ == Meta m _ = n == m

instance Ord (Meta s f) where
  Meta n _ `compare` Meta m _ = compare n m

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

-- re-unifying the term they point to.
semiprune :: (Variable f, Monad f, Traversable f) => f (Meta s f) -> U s (f (Meta s f))
semiprune t0 = case preview var t0 of
  Just v0 -> loop t0 v0
  Nothing -> return t0
  where
    loop t1 v1 = readMeta v1 >>= \mb -> case mb of
      Nothing -> return t1
      Just t  -> case preview var t of
        Nothing -> return t -- t1?
        Just v  -> do
          fv <- loop t v
          writeMeta v1 fv
          return fv
{-# INLINE semiprune #-}

{-

zonkMetaWith :: (Monad f, Traversable f) => f (Meta s f) -> StateT IntSet (U s) (f (Meta s f))
zonkMetaWith fs = for fs $ \mv -> readMeta mv


-- | Expand meta variables recursively
zonkMeta :: (Monad f, Traversable f) => f (Meta s f) -> U s (f (Meta s f))
zonkMeta f = fmap join . for f $ \m -> readMeta m >>= \mv -> case mv of
    Nothing  -> return (return m)
    Just fmf -> do
      r <- zonkMeta fmf -- recursively expand
      writeMeta m r -- save the zonked version back to avoid work later
      return r
-}
