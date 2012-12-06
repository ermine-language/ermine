{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Mangled
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Classes and tools for traversing and binding.
--------------------------------------------------------------------

module Ermine.Mangled
  (
  -- * Bound by another type
    BoundBy(..)
  -- * Mangling
  , Mangled(..)
  , traverseDefault
  , bindDefault
  -- * Mangling by another type
  , MangledBy(..)
  ) where

import Bound
import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Traversable

-- | Perform simultaneous traversal and binding.
class (Traversable t, Applicative t, Monad t) => Mangled t where
  mangled :: Applicative f => (a -> f (t b)) -> t a -> f (t b)
  mangled f m = join <$> traverse f m

instance Mangled [] where
  mangled _ [] = pure []
  mangled f (a:as) = (++) <$> f a <*> mangled f as

instance Mangled Maybe where
  mangled _ Nothing = pure Nothing
  mangled f (Just a) = f a

{-
instance Mangled (Either a) where
  mangled _ (Left a) = pure (Left a)
  mangled f (Right b) = f b
-}

-- | A default definition of 'traverse' defined in terms of 'mangled'
traverseDefault :: (Mangled t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverseDefault f = mangled (fmap pure . f)
{-# INLINE traverseDefault #-}

-- | A default definition of ('>>=') defined in terms of 'mangled'.
bindDefault :: Mangled t => t a -> (a -> t b) -> t b
bindDefault m f = runIdentity (mangled (Identity . f) m)
{-# INLINE bindDefault #-}

-- | Generalizes 'Bound' to permit binding by another type without taking it as a parameter.
class Monad m => BoundBy tm m | tm -> m where
  boundBy :: (a -> m b) -> tm a -> tm b

instance Monad m => BoundBy (Scope b m) m where
  boundBy = flip (>>>=)
  {-# INLINE boundBy #-}

-- | Generalizes 'Mangled' to permit binding by another type without taking it as a parameter.
class (Traversable tm, Mangled m, BoundBy tm m) => MangledBy tm m | tm -> m where
  mangledBy :: Applicative f => (a -> f (m b)) -> tm a -> f (tm b)
  mangledBy f m = boundBy id <$> traverse f m

instance Mangled m => MangledBy (Scope b m) m where
  mangledBy f = fmap Scope . traverse (traverse (mangled f)) . unscope
  {-# INLINE mangledBy #-}
