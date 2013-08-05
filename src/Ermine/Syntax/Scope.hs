{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- This module provides extensions to the @bound@ package.
--------------------------------------------------------------------
module Ermine.Syntax.Scope
  ( hoistScope
  , bitraverseScope
  , transverseScope
  , BoundBy(..)
  , instantiateVars
  -- , putVar , getVar , putScope, getScope
  , serializeScope3
  , deserializeScope3
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Data.Bytes.Serial
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bitraversable

-- | Generalizes 'Bound' to permit binding by another type without taking it as a parameter.
class Monad m => BoundBy tm m | tm -> m where
  boundBy :: (a -> m b) -> tm a -> tm b

instance Monad m => BoundBy (Scope b m) m where
  boundBy = flip (>>>=)
  {-# INLINE boundBy #-}

-- | Lift a natural transformation from @f@ to @g@ into one between scopes.
hoistScope :: Functor f => (forall x. f x -> g x) -> Scope b f a -> Scope b g a
hoistScope t (Scope b) = Scope $ t (fmap t <$> b)
{-# INLINE hoistScope #-}

-- | This allows you to 'bitraverse' a 'Scope'.
bitraverseScope :: (Bitraversable t, Applicative f) => (k -> f k') -> (a -> f a') -> Scope b (t k) a -> f (Scope b (t k') a')
bitraverseScope f g = fmap Scope . bitraverse f (traverse (bitraverse f g)) . unscope
{-# INLINE bitraverseScope #-}

-- | This is a higher-order analogue of 'traverse'.
transverseScope :: (Applicative f, Monad f, Traversable g)
                => (forall r. g r -> f (h r))
                -> Scope b g a -> f (Scope b h a)
transverseScope tau (Scope e) = Scope <$> (tau =<< traverse (traverse tau) e)

-- | instantiate bound variables using a list of new variables
instantiateVars :: Monad t => [a] -> Scope Int t a -> t a
instantiateVars as = instantiate (vs !!) where
  vs = map return as
{-# INLINE instantiateVars #-}

serializeScope3 :: MonadPut m
                => (b -> m ()) -> (forall a. (a -> m ()) -> f a -> m ()) -> (v -> m ())
                -> Scope b f v -> m ()
serializeScope3 pb pf pv (Scope e) = pf (serializeWith2 pb (pf pv)) e

deserializeScope3 :: MonadGet m
                  => m b -> (forall a. m a -> m (f a)) -> m v
                  -> m (Scope b f v)
deserializeScope3 gb gf gv = Scope <$> gf (deserializeWith2 gb (gf gv))
