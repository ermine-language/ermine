{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Unification
-- Copyright :  (c) Edward Kmett 2011-2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Meta variables for unification
--------------------------------------------------------------------
module Ermine.Unification
  (
  -- * Our unification monad
    U, evalU
  , fresh, st
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.ST
import Ermine.Rendering

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
