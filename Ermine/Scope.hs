{-# LANGUAGE Rank2Types #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Scope
-- Copyright :  (c) Edward Kmett
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: portable
--------------------------------------------------------------------
module Ermine.Scope
  ( hoistScope
  , bitraverseScope -- diverseScope?
  , triverseScope
  ) where

import Bound
import Control.Applicative
import Data.Traversable
import Data.Bitraversable
import Ermine.Trifunctor

hoistScope :: Functor f => (forall x. f x -> g x) -> Scope b f a -> Scope b g a
hoistScope t (Scope b) = Scope $ t (fmap t <$> b)

bitraverseScope :: (Bitraversable t, Applicative f) => (k -> f k') -> (a -> f a') -> Scope b (t k) a -> f (Scope b (t k') a')
bitraverseScope f g = fmap Scope . bitraverse f (traverse (bitraverse f g)) . unscope

triverseScope :: (Triversable e, Applicative f) => (k -> f k') -> (t -> f t') -> (a -> f a') -> Scope b (e k t) a -> f (Scope b (e k' t') a')
triverseScope f g h = fmap Scope . triverse f g (traverse (triverse f g h)) . unscope
