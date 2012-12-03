{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Trifunctor
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Ermine.Trifunctor
  ( Trifunctor(..)
  , Trifoldable(..)
  , Triversable(..)
  ) where

import Data.Monoid
import Control.Applicative
import Data.Functor.Identity

class Trifunctor l where
  trimap :: (k -> k') -> (t -> t') -> (a -> a') -> l k t a -> l k' t' a'
  default trimap :: Triversable l => (k -> k') -> (t -> t') -> (a -> a') -> l k t a -> l k' t' a'
  trimap f g h = runIdentity . triverse (Identity . f) (Identity . g) (Identity . h)

instance Trifunctor (,,) where
  trimap f g h (a,b,c) = (f a, g b, h c)

class Trifoldable l where
  trifoldMap :: Monoid m => (k -> m) -> (t -> m) -> (a -> m) -> l k t a -> m
  default trifoldMap :: (Triversable l, Monoid m) => (k -> m) -> (t -> m) -> (a -> m) -> l k t a -> m
  trifoldMap f g h = getConst . triverse (Const . f) (Const . g) (Const . h)

instance Trifoldable (,,) where
  trifoldMap f g h (a,b,c) = f a <> g b <> h c

class (Trifunctor l, Trifoldable l) => Triversable l where
  triverse :: Applicative f => (k -> f k') -> (t -> f t') -> (a -> f a') -> l k t a -> f (l k' t' a')

instance Triversable (,,) where
  triverse f g h (a,b,c) = (,,) <$> f a <*> g b <*> h c
